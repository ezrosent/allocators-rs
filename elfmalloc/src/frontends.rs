// Copyright 2017 the authors. See the 'Copyright and license' section of the
// README.md file at the top-level directory of this repository.
//
// Licensed under the Apache License, Version 2.0 (the LICENSE-APACHE file) or
// the MIT license (the LICENSE-MIT file) at your option. This file may not be
// copied, modified, or distributed except according to those terms.

//! Frontends built on top of the `Slag` subsystem.
//!
//! By frontend, we mean some thread-specific data wrapping the shared infrastructure built in the
//! `Slag` module. These frontends are able to provide fast, multithreaded alloaction routines for
//! a particular object size. These object-specific allocators can be used as a specialized
//! allocator, but their main use-case is as a building block for a general dynamic allocator. This
//! latter task is implemented in the `general` module.
use std::cmp;
use std::intrinsics::likely;
use std::marker::PhantomData;
use std::mem;
use std::ptr::{self, NonNull};

use alloc::allocator::Layout;
use backing::{new_marked_mmap_cache, MarkedAlloc, MarkedMmapCache, PageCache};
use crossbeam_epoch::{Collector, Handle};
use mmap_alloc::{MapAlloc, MapAllocBuilder};
use object_alloc::UntypedObjectAlloc;

use alloc_type::AllocType;
use slag::*;
use util::{as_mut, as_ref, mmap, AllocWith, TryCloneWith, OwnedArray, LazyInitializable};

pub trait Frontend
where Self: Sized + AllocWith + TryCloneWith
{
    fn new<P: UntypedObjectAlloc>(handle: &Handle, pages: &mut P, alloc: SlagAllocator) -> Option<Self>;
}

/// A `LocalCache` provides thread-local data on top of a `SlagAllocator`.
///
/// Like a `MagazineCache`, it includes a stack of pointers to cache allocations. Unlike
/// `MagazineCache`, only pointers local to the current `Slag` are placed in this stack: the rest
/// are eagerly freed back to their owning `Slag`. Generally speaking, `LocalCache`s are better at
/// producer-consumer workloads and worse at predominantly thread-local workloads than the
/// `MagazineCache`. `LocalCache`s also have stronger guarantees when it comes to object locality:
/// almost all adjacent allocations will come from the same region of memory.
pub struct LocalCache {
    alloc: SlagAllocator,
    vals: PtrStack,
    iter: AllocIter,
    #[cfg(debug_assertions)]
    dropped: bool,
}

#[cfg(debug_assertions)]
impl Drop for LocalCache {
    fn drop(&mut self) {
        alloc_debug_assert!(self.dropped);
    }
}

impl TryCloneWith for LocalCache {
    fn try_clone_with<P: UntypedObjectAlloc>(&self, handle: &Handle, pages: &mut P) -> Option<Self> {
        LocalCache::new(handle, pages, self.alloc.clone())
    }
}

impl Frontend for LocalCache {
    fn new<P: UntypedObjectAlloc>(handle: &Handle, pages: &mut P, mut alloc: SlagAllocator) -> Option<Self> {
        unsafe {
            let stack = PtrStack::new((*alloc.m.as_ptr()).n_objects);
            let iter = alloc.refresh(handle, pages)?;
            Some(LocalCache {
                alloc,
                vals: stack,
                iter,
                #[cfg(debug_assertions)]
                dropped: false,
            })
        }
    }
}

unsafe impl AllocWith for LocalCache {
    unsafe fn alloc_with<P: UntypedObjectAlloc>(&mut self, handle: &Handle, pages: &mut P) -> Option<NonNull<u8>> {
        self.vals
            .pop()
            .or_else(|| self.iter.next())
            .or_else(|| {
                self.iter = self.alloc.refresh(handle, pages)?;
                Some(self.iter.next().expect("new iterator should have values"))
            })
    }

    unsafe fn dealloc_with<P: UntypedObjectAlloc>(&mut self, handle: &Handle, pages: &mut P, it: NonNull<u8>) {
        if self.alloc.contains(it) {
            self.vals.push(it);
        } else {
            self.alloc.dealloc(handle, pages, it);
        }
    }

    /// Free all resources in preparation for dropping `self`.
    /// 
    /// Since freeing the resources in a `LocalCache` requires both a Crossbeam
    /// epoch GC handle and a page allocator, neither of which are stored
    /// internally, the freeing logic cannot go in `Drop::drop`, and thus the
    /// owner of this `LocalCache` is responsible for calling `pre_drop` before
    /// dropping it.
    unsafe fn pre_drop<P: UntypedObjectAlloc>(&mut self, handle: &Handle, pages: &mut P) {
        alloc_debug_assert!(!self.dropped);
        #[cfg(debug_assertions)]
        {
            self.dropped = true;
        }
        unsafe {
            let meta = as_ref(self.alloc.m);
            let mask = self.iter.cur_word;
            let word = NonNull::new_unchecked(self.iter.next_word.offset(-1));
            let slag = self.alloc.slag;
            self.alloc.bulk_dealloc(handle, pages, mask, word, slag, meta);
            for i in 0..self.vals.top {
                let item = *self.vals.data.get(i);
                self.alloc.dealloc(handle, pages, item)
            }
        }
    }
}

/// A different approach to caching to `LocalCache` inspired by Bonwick-style magazines.
///
/// The advantage `MagazineCache` has over `LocalCache` is that it will unconditionally push to a
/// local data-structure if there is room (it doesn't have to be a local free). The magazine
/// structure also allows us to batch together remote frees (by simply composing bit-set masks
/// ahead of a fetch-or), reducing the number of atomic instruction that must be issued for most
/// remote frees.
pub struct MagazineCache {
    stack_size: usize,
    s: PtrStack,
    iter: AllocIter,
    alloc: SlagAllocator,
    coalescer: Coalescer,
    #[cfg(debug_assertions)]
    dropped: bool,
}

impl LazyInitializable for MagazineCache {
    type Params = (NonNull<Metadata>, usize, RevocablePipe<Slag>);
    fn init<P: UntypedObjectAlloc>(&(meta, decommit, ref avail): &Self::Params, handle: &Handle, pages: &mut P) -> Option<Self> {
        let alloc = SlagAllocator::partial_new(pages, meta, decommit, avail.clone())?;
        Some(Self::new(handle, pages, alloc)?)
    }
}

impl LazyInitializable for LocalCache {
    type Params = (NonNull<Metadata>, usize, RevocablePipe<Slag>);
    fn init<P: UntypedObjectAlloc>(&(meta, decommit, ref avail): &Self::Params, handle: &Handle, pages: &mut P) -> Option<Self> {
        let alloc = SlagAllocator::partial_new(pages, meta, decommit, avail.clone())?;
        Some(Self::new(handle, pages, alloc)?)
    }
}

#[cfg(debug_assertions)]
impl Drop for MagazineCache {
    fn drop(&mut self) {
        alloc_debug_assert!(self.dropped)
    }
}

impl TryCloneWith for MagazineCache {
    fn try_clone_with<P: UntypedObjectAlloc>(&self, handle: &Handle, pages: &mut P) -> Option<Self> {
        MagazineCache::new_sized(handle, pages, self.alloc.clone(), self.stack_size)
    }
}

impl MagazineCache {
    pub fn new_sized<P: UntypedObjectAlloc>(handle: &Handle, pages: &mut P, mut alloc: SlagAllocator, magazine_size: usize) -> Option<Self> {
        alloc_assert!(magazine_size > 0);
        let s = PtrStack::new(magazine_size);
        let iter = unsafe { alloc.refresh(handle, pages)? };
        let buckets = Coalescer::new(magazine_size * 2);
        Some(MagazineCache {
            stack_size: magazine_size,
            s,
            iter,
            alloc,
            coalescer: buckets,
            #[cfg(debug_assertions)]
            dropped: false,
        })
    }

    /// Allocate memory from the current owned `Slag`.
    ///
    /// This amounts to getting memory from the current alloc iterator. If the iterator is
    /// exhausted, a new `Slag` is acquired.
    unsafe fn slag_alloc<P: UntypedObjectAlloc>(&mut self, handle: &Handle, pages: &mut P) -> Option<NonNull<u8>> {
        for _ in 0..2 {
            match self.iter.next() {
                Some(ptr) => return Some(ptr),
                None => self.iter = self.alloc.refresh(handle, pages)?,
            }
        }
        alloc_panic!(
            "New slag is empty {:?} {:?}",
            self.alloc.slag,
            self.alloc.slag.as_ref().rc.load()
        )
    }

    /// Perform the bulk-level frees for the `Coalescer`.
    unsafe fn return_memory<P: UntypedObjectAlloc>(&mut self, handle: &Handle, pages: &mut P) {
        alloc_debug_assert_eq!(self.s.top as usize, self.stack_size);
        let new_top = self.stack_size / 2;
        let meta = as_ref(self.alloc.m);
        // iterate over the stack and attempt to add them to the coalescer.
        for i in new_top..self.stack_size {
            let item = *self.s.data.get(i);
            if !self.coalescer.insert(item, meta) {
                // there was a "hash collision", so we simply free `item` directly
                self.alloc.dealloc(handle, pages, item)
            }
        }
        self.s.top = new_top;
        for cell_ptr in 0..self.coalescer.1.top {
            let cell = &mut **(self.coalescer.1.data.get(cell_ptr) as *mut *mut RemoteFreeCell);
            // Slag::find will technically work if you hand it any pointer within the slag
            // itself, not just an object. As a result, we use the reference count to get at
            // the slag it belongs to.
            let slag = Slag::find(NonNull::new_unchecked(cell.rc as *mut _), meta.total_bytes);
            let word = NonNull::new_unchecked(cell.word);
            self.alloc.bulk_dealloc(handle, pages, cell.mask, word, slag, meta);
            ptr::write(cell, RemoteFreeCell::default());
        }
        self.coalescer.1.top = 0;
    }
}

impl Frontend for MagazineCache {
    fn new<P: UntypedObjectAlloc>(handle: &Handle, pages: &mut P, alloc: SlagAllocator) -> Option<Self> {
        use std::cmp;
        let object_size = unsafe { alloc.m.as_ref().object_size };
        const CUTOFF: usize = 32 << 10;
        let magazine_size = match object_size {
            0...512 => 1 << 16,
            513...CUTOFF => 512 << 10 / object_size,
            _ => 1 << 20 / object_size,
        };
        Self::new_sized(handle, pages, alloc, cmp::max(1, magazine_size))
    }
}

unsafe impl AllocWith for MagazineCache {
    unsafe fn alloc_with<P: UntypedObjectAlloc>(&mut self, handle: &Handle, pages: &mut P) -> Option<NonNull<u8>> {
        if let Some(ptr) = self.s.pop() {
            trace_event!(cache_alloc);
            Some(ptr)
        } else {
            trace_event!(slag_alloc);
            self.slag_alloc(handle, pages)
        }
    }

    unsafe fn dealloc_with<P: UntypedObjectAlloc>(&mut self, handle: &Handle, pages: &mut P, item: NonNull<u8>) {
        trace_event!(local_free);
        if likely(self.s.top < self.stack_size) {
            self.s.push(item);
            return;
        }
        self.return_memory(handle, pages);
        self.s.push(item);
    }

    /// Free all resources in preparation for dropping `self`.
    /// 
    /// Since freeing the resources in a `MagazineCache` requires both a
    /// Crossbeam epoch GC handle and a page allocator, neither of which are
    /// stored internally, the freeing logic cannot go in `Drop::drop`, and
    /// thus the owner of this `MagazineCache` is responsible for calling
    /// `pre_drop` before dropping it.
    unsafe fn pre_drop<P: UntypedObjectAlloc>(&mut self, handle: &Handle, pages: &mut P) {
        alloc_debug_assert!(!self.dropped);
        #[cfg(debug_assertions)]
        {
            self.dropped = true;
        }
        unsafe {
            let meta = as_ref(self.alloc.m);
            let mask = self.iter.cur_word;
            let word = NonNull::new_unchecked(self.iter.next_word.offset(-1));
            let slag = self.alloc.slag;
            // bulk-free the current AllocIter word. Then free all elements in the magazine.
            self.alloc.bulk_dealloc(handle, pages, mask, word, slag, meta);
            for i in 0..self.s.top {
                let item = *self.s.data.get(i);
                self.alloc.dealloc(handle, pages, item)
            }
        }
    }
}

/// A set data-structure used to batch remote free operations.
struct Coalescer(MmapVec<RemoteFreeCell>, PtrStack);

/// The internal data for a `Coalescer`.
///
/// This holds all the provenance necessary to perform a batched remote free operation.
struct RemoteFreeCell {
    // TODO: explore using slag-specific information as well as word-specific.
    // Idea: have the "key" be rc, and then use a *mut Slag to get at the entire bitset, along with
    // an array of masks, then you can go as before, but only increment ref count once.
    /// The reference count to be updated in accordance for the accumulated `mask`.
    rc: *mut RefCount,
    /// The specific bitset word to be or-ed with `mask`.
    word: *mut Word,
    /// The accumulated mask to update `word`.
    mask: usize,
}

impl Default for RemoteFreeCell {
    fn default() -> Self {
        RemoteFreeCell {
            rc: ptr::null_mut(),
            word: ptr::null_mut(),
            mask: 0,
        }
    }
}

impl Coalescer {
    fn new(size: usize) -> Self {
        Coalescer(
            MmapVec::new(size.next_power_of_two()),
            PtrStack::new(size),
        )
    }

    fn bucket_num(&self, word: usize) -> usize {
        // we can do a "fast mod" operation because we know cap() is a power of two (see `new`)
        word & (self.0.cap() - 1)
    }

    /// Try to insert `item`.
    ///
    /// The return value indicates if the value was successfully inserted.
    unsafe fn insert(&mut self, item: NonNull<u8>, meta: &Metadata) -> bool {
        fn hash_ptr(p: *mut Word) -> usize {
            let p_num = p as usize;
            let words = p_num >> 3;
            let pages = words >> 18;
            pages.wrapping_mul(words)
        }
        let s = Slag::find(item, meta.total_bytes);
        let rc_ptr = &s.as_ref().rc as *const _ as *mut RefCount;
        let (word, word_ix) = Slag::get_word(s, item, meta);
        let word_ptr = ((s.as_ptr() as *mut u8).offset(meta.bitset_offset) as *mut Word)
            .offset(word);
        let bucket_ind = self.bucket_num(hash_ptr(word_ptr));
        let bucket = &mut *self.0.get_debug_checked(bucket_ind);
        // XXX: using the property of the creek implementation that it fills newly-dirtied pages
        // with zeros.
        if bucket.rc.is_null() {
            *bucket = RemoteFreeCell {
                rc: rc_ptr,
                word: word_ptr,
                mask: 1 << word_ix,
            };
            // append this to a stack of buckets we know are non-null
            self.1.push(NonNull::from(bucket).cast());
            return true;
        }
        if bucket.word == word_ptr {
            bucket.mask |= 1 << word_ix;
            return true;
        }
        false
    }
}
/// A thread-local stack data-structure for caching allocations from an owned `Slag`.
///
/// This implementation is specialized in a few ways:
///
/// * While `mem` has a maximum size, it is assumed that all `push` calls will leave the
///   stack in a valid state. This is because the stack is initialized to have sufficient space
///   for an entire `Slag` of pointers for this particular size class, and all `push`es only
///   happen after the item in question has been confirmed to belong to a particular `Slag`.
///   Additional checking must be implemented on top of `PtrStack` if these are not the desired
///   semantics. See the `MagazineCache` implementation for an example of this.
///
/// * The stack is backed by an `OwnedArray`, which manages its own memory through `mmap`. This has
///   a number of advantages.  It reduces reliance on `Vec`-like structures that are tied to the
///   underlying `malloc` implementation. It also gives us lazy initialization without any extra
///   work. Fresh  will be uncommited: this means that potentially large allocations of memory for
///   stacks will only consume physical space when they are used.
struct PtrStack {
    data: MmapVec<NonNull<u8>>,
    top: usize,
}

impl PtrStack {
    fn new(max_objects: usize) -> PtrStack {
        PtrStack {
            data: MmapVec::new(max_objects),
            top: 0,
        }
    }

    unsafe fn push(&mut self, item: NonNull<u8>) {
        *self.data.get_debug_checked(self.top) = item;
        self.top += 1;
    }

    unsafe fn pop(&mut self) -> Option<NonNull<u8>> {
        if self.empty() {
            None
        } else {
            self.top -= 1;
            Some(*self.data.get(self.top))
        }
    }

    #[inline]
    fn empty(&self) -> bool {
        self.top == 0
    }
}

pub use self::magazine::{Depot, DepotCache};

mod magazine {
    //! A more direct port of
    //! [Bonwick et al.'s "Magazine" algorithm](https://www.usenix.org/legacy/event/usenix01/full_papers/bonwick/bonwick.pdf)
    //! used in the Solaris slab allocator. Adding a magazine layer can be used to improve
    //! allocation and deallocation speeds at the cost of reduced locality and higher memory
    //! overhead when used for smaller objects.
    //!
    //! # Overview
    //! Briefly, a `DepotCache` is a generic caching layer implemented in terms of a pre-existing
    //! `Frontend` implementation. This layer consists of `BagPipe`s of `Magazine`s. Each
    //! `Magazine` is a fixed-size stack of pointers to objects of a particular size. Each thread
    //! has two `Magazine`s (`m1` and `m2`).
    //!
    //! ## Allocation
    //! To allocate memory, a thread first attempts to pop an object from `m1`. If this fails, `m1`
    //! and `m2` are swapped, and allocation is retried. If this fails for a second time it means
    //! that both magazines are empty. At this point, `m1` is exchanged for a new (full) `Magazine`
    //! using the `Depot` and allocation is retried. If this latter step fails, `m1` is re-filled
    //! using the underlying `Frontend`. Note that the presence of two `Magazine`s ensures that the
    //! `Depot` is only accessed at a minimum of every *k* allocations, where *k* is the size of a
    //! given `Magazine`.
    //!
    //! ## Deallocation
    //! Deallocation works in a symmetric fashion, where `m1` is returned to the `Depot` if both
    //! `Magazine`s are full, and `m1` is freed back to the underlying `Frontend` if the `Depot`
    //! cannot accept any new `Magzine`s.
    //!
    //! ## Difference from Original.
    //! The main difference here is that we do not allow magazines to grow dynamically. This is
    //! because we implement the `Depot` using a `BagPipe`; as a result, the `Depot` should not be
    //! a source of contention except in the case where magazines are quite small.

    use super::*;
    use super::super::bagpipe::bag::WeakBag;
    use super::super::bagpipe::{BagPipe, BagCleanup};
    use super::super::bagpipe::queue::FAAQueueLowLevel;

    /// A Custom destructor for Magazines in a `BagPipe`.
    #[derive(Copy, Clone, Default)]
    struct MagazineCleanup;
    impl BagCleanup for MagazineCleanup {
        type Item = *mut Magazine;
        fn cleanup(&self, it: *mut Magazine) {
            unsafe { Magazine::destroy(it) }
        }
    }

    type MagPipe = BagPipe<FAAQueueLowLevel<*mut Magazine>, MagazineCleanup>;

    /// A fixed-size stack backed by `mmap`.
    ///
    /// A `Magazine` is a lot like a `PtrStack`. The primary difference is that a `Magazine` stores
    /// its data inline, whereas the `PtrStack` has data intended to be allocated on the stack, or
    /// as part of thread-local storage.
    struct Magazine {
        top: usize,
        cap: usize,
        mapped: usize,
        base: *mut u8,
    }

    impl Magazine {
        unsafe fn new(size: usize) -> *mut Magazine {
            let page_size = mmap::page_size();
            alloc_debug_assert!(page_size.is_power_of_two());
            let bytes = mem::size_of::<Magazine>() + mem::size_of::<*mut u8>() * size;
            let rem = bytes & (page_size - 1);
            let n_pages = (bytes >> page_size.trailing_zeros()) + cmp::min(1, rem);
            let region_size = n_pages * page_size;
            alloc_debug_assert!(bytes <= region_size);
            let mem = mmap::map(region_size) as *mut Magazine;
            ptr::write(
                mem,
                Magazine {
                    top: 0,
                    cap: size,
                    mapped: region_size,
                    base: mem.offset(1) as *mut u8,
                },
            );
            mem
        }

        unsafe fn default() -> *mut Magazine {
            let res = Magazine::new(3068);
            res
        }

        /// Unmap the memory associated with the `Magazine`.
        ///
        /// This function does not free the memory associated with the stack's contents if it is
        /// nonempty.
        unsafe fn destroy(slf: *mut Magazine) {
            alloc_debug_assert_eq!(((*slf).base as *mut Magazine).offset(-1), slf);
            alloc_debug_assert_eq!(slf as usize % mmap::page_size(), 0);
            mmap::unmap(slf as *mut u8, (*slf).mapped)
        }

        fn push(&mut self, item: NonNull<u8>) -> bool {
            if self.top == self.cap {
                return false;
            }
            unsafe {
                let addr = (self.base as *mut *mut u8).offset(self.top as isize);
                alloc_debug_assert!((addr as isize - self.base as isize) < self.mapped as isize);
                ptr::write(addr, item.as_ptr());
            }
            self.top += 1;
            true
        }

        fn pop(&mut self) -> Option<NonNull<u8>> {
            if self.top == 0 {
                return None;
            }
            unsafe {
                self.top -= 1;
                let res = ptr::read((self.base as *mut *mut u8).offset(self.top as isize));
                alloc_debug_assert!(!res.is_null());
                Some(NonNull::new_unchecked(res))
            }
        }
    }

    /// A global cache for empty and full `Magazine`s.
    #[derive(Clone)]
    pub struct Depot {
        max_size: isize,
        empty: MagPipe,
        full: MagPipe,
    }

    impl Default for Depot {
        fn default() -> Depot {
            Depot::new()
        }
    }

    impl Depot {
        fn new_size(max_size: usize, empty: usize, full: usize) -> Depot {
            alloc_assert!(max_size < (isize::max_value() as usize));
            Depot {
                max_size: max_size as isize,
                empty: MagPipe::new_size(empty),
                full: MagPipe::new_size(full),
            }
        }

        fn new() -> Depot {
            use super::super::num_cpus;
            Depot::new_size(1 << 20, num_cpus::get(), num_cpus::get())
        }

        /// Return an empty `Magazine` to the `Depot`.
        ///
        /// If the `Depot` is at capacity, the `Magazine`'s memory is unmapped.
        unsafe fn free_empty(&mut self, m: *mut Magazine) {
            alloc_debug_assert_eq!((*m).top, 0);
            if self.empty.size_guess() >= self.max_size {
                Magazine::destroy(m);
            } else {
                self.empty.push_mut(m)
            }
        }

        /// Return a full `Magazine` to the `Depot`.
        ///
        /// If the `Depot` is at capacity, the `Magazine` is not added to the `Depot` and this
        /// method returns `false`.
        fn free_full(&mut self, m: *mut Magazine) -> bool {
            unsafe {
                alloc_debug_assert_eq!((*m).top, (*m).cap)
            };
            if self.full.size_guess() >= self.max_size {
                false
            } else {
                self.full.push_mut(m);
                true
            }
        }

        /// Allocate a full `Magazine` from the `Depot` if one is present.
        fn alloc_full(&mut self) -> Option<*mut Magazine> {
            self.full.pop_mut().and_then(|r| {
                unsafe {
                    alloc_debug_assert_eq!((*r).top, (*r).cap)
                };
                Some(r)
            })
        }

        /// Allocate a full `Magazine` from the `Depot`, constructing a new one if none are
        /// present.
        fn alloc_empty(&mut self) -> *mut Magazine {
            let res = self.empty.pop_mut().unwrap_or_else(|| {
                unsafe {
                    Magazine::default()
                }
            });
            unsafe {
                alloc_debug_assert_eq!((*res).top, 0)
            };
            res
        }

        /// Swap out an empty `Magazine` for a full one.
        ///
        /// If a full `Magazine` is unavailable, `None` is returned and `m` is not freed.
        fn swap_empty(&mut self, m: *mut Magazine) -> Option<*mut Magazine> {
            self.alloc_full().and_then(|new_m| {
                unsafe {
                    self.free_empty(m)
                };
                Some(new_m)
            })
        }

        /// Swap out a full `Magazine` for an empty one.
        ///
        /// If the `Depot` is at capacity, `None` is returned and `m` is not freed.
        fn swap_full(&mut self, m: *mut Magazine) -> Option<*mut Magazine> {
            if self.free_full(m) {
                Some(self.alloc_empty())
            } else {
                None
            }
        }
    }

    /// A magazine caching layer on top of a given frontend.
    ///
    /// See module comments for more details on this algorithm.
    #[derive(Clone)]
    pub struct DepotCache<A: AllocWith> {
        backing: A,
        depot: Depot,
        m1: *mut Magazine,
        m2: *mut Magazine,
        #[cfg(debug_assertions)]
        dropped: bool,
    }

    impl<A: AllocWith + LazyInitializable> LazyInitializable for DepotCache<A> {
        type Params = (A::Params, Depot);
        fn init<P: UntypedObjectAlloc>(&(ref backing, ref depot): &Self::Params, handle: &Handle, pages: &mut P) -> Option<DepotCache<A>> {
            Some(Self::new(A::init(backing.clone(), handle, pages)?, depot.clone()))
        }
    }

    impl<A: AllocWith> DepotCache<A> {
        fn new(backing: A, mut depot: Depot) -> DepotCache<A> {
            let m1 = depot.alloc_full().unwrap_or_else(|| depot.alloc_empty());
            let m2 = depot.alloc_empty();
            DepotCache { backing, depot, m1, m2, dropped: false }
        }
    }

    unsafe impl<A: AllocWith> AllocWith for DepotCache<A> {
        unsafe fn alloc_with<P: UntypedObjectAlloc>(&mut self, handle: &Handle, pages: &mut P) -> Option<NonNull<u8>> {
            if let Some(p) = (*self.m1).pop() {
                return Some(p);
            }
            mem::swap(&mut self.m1, &mut self.m2);
            if let Some(p) = (*self.m1).pop() {
                return Some(p);
            }

            if let Some(m) = self.depot.swap_empty(self.m1) {
                self.m1 = m;
            } else {
                let cap = (*self.m1).cap;
                for _ in 0..cap {
                    let _r = (*self.m1).push(self.backing.alloc_with(handle, pages)?);
                    alloc_debug_assert!(_r);
                }
            }
            Some((*self.m1).pop().expect("new full magazine is empty"))
        }

        unsafe fn dealloc_with<P: UntypedObjectAlloc>(&mut self, handle: &Handle, pages: &mut P, item: NonNull<u8>) {
            if (*self.m1).push(item) {
                return;
            }
            mem::swap(&mut self.m1, &mut self.m2);

            if (*self.m1).push(item) {
                return;
            }
            match self.depot.swap_full(self.m1) {
                Some(m) => self.m1 = m,
                None => {
                    while let Some(x) = (*self.m1).pop() {
                        self.backing.dealloc(handle, pages, x)
                    }
                },
            };
            let _r = (*self.m1).push(item);
            alloc_debug_assert!(_r);
        }

        unsafe fn pre_drop<P: UntypedObjectAlloc>(&mut self, handle: &Handle, pages: &mut P) {
            alloc_debug_assert!(self.dropped);
            #[cfg(debug_assertions)]
            {
                self.dropped = true;
            }
            for m in &[self.m1, self.m2] {
                let m_raw: *mut Magazine = *m;
                unsafe {
                    while let Some(p) = (*m_raw).pop() {
                        self.backing.dealloc(handle, pages, p);
                    }
                    self.depot.free_empty(m_raw);
                }
            }
        }
    }

    #[cfg(debug_assertions)]
    impl<A: AllocWith> Drop for DepotCache<A> {
        fn drop(&mut self) {
            alloc_debug_assert!(self.dropped);
        }
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        #[test]
        fn magazine_stack() {
            unsafe {
                let m = Magazine::default();
                let m_ref = &mut*m;
                let goal = m_ref.cap;
                // start at 1 because we debug_assert that the pointer is non-null
                for i in 1..(goal + 1) {
                    alloc_assert!(m_ref.push(i as *mut u8));
                }
                alloc_assert!(!m_ref.push(8 as *mut u8));
                alloc_assert_eq!(m_ref.pop(), Some(m_ref.cap as *mut u8));
                alloc_assert!(m_ref.push(8 as *mut u8));
                Magazine::destroy(m);
            }
        }
    }
}

/// A builder-pattern-style builder for `MagazineAllocator`s and `LocalAllocator`s.
///
/// ```rust,ignore
/// // A usize-specific allocator using the `LocalCache` frontend, customized to use 32K pages.
/// let la: LocalAllocator<usize> = AllocBuilder::default().page_size(32 << 10).build_local();
/// ```
///
/// Modifying other builder parameters past the default is not recommended. The overall API is
/// unstable.
pub struct AllocBuilder<T> {
    cutoff_factor: f64,
    page_size: usize,
    target_overhead: usize,
    eager_decommit_threshold: usize,
    max_objects: usize,
    _marker: PhantomData<T>,
}

impl<T> Default for AllocBuilder<T> {
    fn default() -> Self {
        AllocBuilder {
            cutoff_factor: 0.6,
            page_size: cmp::max(32 << 10, mem::size_of::<T>() * 4),
            target_overhead: 1 << 20,
            eager_decommit_threshold: 128 << 10,
            max_objects: 1 << 30,
            _marker: PhantomData,
        }
    }
}

impl<T> AllocBuilder<T> {
    pub fn cutoff_factor(&mut self, cutoff_factor: f64) -> &mut Self {
        self.cutoff_factor = cutoff_factor;
        self
    }
    pub fn page_size(&mut self, page_size: usize) -> &mut Self {
        self.page_size = page_size;
        self
    }
    pub fn target_overhead(&mut self, target_overhead: usize) -> &mut Self {
        self.target_overhead = target_overhead;
        self
    }
    pub fn eager_decommit_threshold(&mut self, eager_decommit_threshold: usize) -> &mut Self {
        self.eager_decommit_threshold = eager_decommit_threshold;
        self
    }
    pub fn max_objects(&mut self, max_objects: usize) -> &mut Self {
        self.max_objects = max_objects;
        self
    }

    /// Build a `LocalAllocator<T>` from the current configuration.
    pub fn build_local(&self) -> Option<LocalAllocator<T>> {
        LocalAllocator::new_standalone(
            self.cutoff_factor,
            self.page_size,
            self.target_overhead,
            self.eager_decommit_threshold,
            self.max_objects,
        )
    }

    /// Build a `MagazineAllocator<T>` from the current configuration.
    pub fn build_magazine(&self) -> Option<MagazineAllocator<T>> {
        MagazineAllocator::new_standalone(
            self.cutoff_factor,
            self.page_size,
            self.target_overhead,
            self.eager_decommit_threshold,
            self.max_objects,
        )
    }
}

macro_rules! typed_wrapper {
    ($name:ident, $wrapped:tt) => {
        pub struct $name<T>{
            cache: $wrapped,
            handle: Handle,
            pages: PageCache<MapAlloc>,
            _marker: PhantomData<T>,
        }

        impl<T> $name<T> {
            pub fn new_standalone(cutoff_factor: f64,
                                  page_size: usize,
                                  target_overhead: usize,
                                  eager_decommit: usize,
                                  max_objects: usize)
                -> Option<Self> {

                    let layout = Layout::from_size_align(page_size, page_size).unwrap();
                    let pages = MapAllocBuilder::default()
                                    .obj_size(page_size)
                                    .obj_align(page_size)
                                    .build();
                    let handle = Collector::new().handle();
                    let mut pages = PageCache::new(pages, 8);                    
                    // let pa = PageCache::new(page_size, target_overhead, 8, AllocType::SmallSlag);
                    let slag = SlagAllocator::new(
                                    &mut pages,
                                    max_objects,
                                    mem::size_of::<T>(),
                                    0,
                                    cutoff_factor,
                                    eager_decommit
                                )?;
                    Some($name {
                        cache: $wrapped::new(&handle, &mut pages, slag)?,
                        handle,
                        pages,
                        _marker: PhantomData,
                    })
                }

            pub unsafe fn alloc(&mut self) -> Option<NonNull<T>> {
                self.cache.alloc(&self.handle, &mut self.pages).map(NonNull::cast)
            }

            pub unsafe fn dealloc(&mut self, item: NonNull<T>) {
                self.cache.dealloc(&self.handle, &mut self.pages, item.cast())
            }
        }

        unsafe impl<T> Send for $name<T> {}
    };
}

typed_wrapper!(LocalAllocator, LocalCache);
typed_wrapper!(MagazineAllocator, MagazineCache);

#[cfg(test)]
mod tests {
    extern crate env_logger;
    use super::*;
    use std::thread;
    use std::ptr::write_volatile;
    use std::collections::HashSet;

    #[test]
    fn obj_alloc_basic() {
        let _ = env_logger::init();
        let mut oa = AllocBuilder::<usize>::default()
            .page_size(4096)
            .build_local();
        unsafe {
            let item = oa.alloc();
            write_volatile(item, 10);
            oa.free(item);
        }
    }

    #[test]
    fn obj_alloc_many_pages_single_threaded_usize() {
        obj_alloc_many_pages_single_threaded::<usize>();
    }

    #[test]
    fn obj_alloc_many_pages_single_threaded_u24() {
        obj_alloc_many_pages_single_threaded::<[u8; 24]>();
    }

    #[test]
    fn obj_alloc_many_pages_single_threaded_u32() {
        obj_alloc_many_pages_single_threaded::<[u8; 32]>();
    }

    fn obj_alloc_many_pages_single_threaded<T: 'static>() {
        let _ = env_logger::init();
        const N_ITEMS: usize = 4096 * 20;
        let mut oa = AllocBuilder::<T>::default().page_size(4096).build_local();
        alloc_assert!(mem::size_of::<T>() >= mem::size_of::<usize>());
        // stay in a local cache
        for _ in 0..N_ITEMS {
            unsafe {
                let item = oa.alloc();
                write_volatile(item as *mut usize, 10);
                oa.free(item);
            }
        }

        let mut v = Vec::with_capacity(N_ITEMS);
        let mut h = HashSet::new();
        for i in 0..N_ITEMS {
            unsafe {
                let item = oa.alloc();
                write_volatile(item as *mut usize, i + 1);
                v.push(item);
                let item_num = item as usize;
                alloc_assert!(!h.contains(&item_num));
                h.insert(item_num);
            }
        }

        for i in v {
            unsafe {
                oa.free(i);
            }
        }
    }
    #[test]
    fn obj_alloc_many_pages_many_threads_usize() {
        obj_alloc_many_pages_many_threads::<usize>()
    }

    #[test]
    fn obj_alloc_many_pages_many_threads_u24() {
        obj_alloc_many_pages_many_threads::<[u8; 24]>()
    }

    #[test]
    fn obj_alloc_many_pages_many_threads_u32() {
        obj_alloc_many_pages_many_threads::<[u8; 32]>()
    }

    fn obj_alloc_many_pages_many_threads<T: 'static>() {
        let _ = env_logger::init();
        use std::mem;
        const N_ITEMS: usize = 4096 * 4;
        const N_THREADS: usize = 40;
        // TODO make macros for these tests and test both MagazineAllocator and LocalAllocator
        let oa = AllocBuilder::<T>::default()
            .page_size(4096)
            .build_magazine();
        // stay in a local cache
        alloc_assert!(mem::size_of::<T>() >= mem::size_of::<usize>());
        let mut threads = Vec::new();
        for _ in 0..N_THREADS {
            let mut my_alloc = oa.clone();
            threads.push(thread::spawn(move || {
                for _ in 0..N_ITEMS {
                    unsafe {
                        let item = my_alloc.alloc();
                        write_volatile(item as *mut usize, 10);
                        my_alloc.free(item);
                    }
                }

                let mut v = Vec::with_capacity(N_ITEMS);
                let mut h = HashSet::new();
                for i in 0..N_ITEMS {
                    unsafe {
                        let item = my_alloc.alloc();
                        write_volatile(item as *mut usize, i);
                        v.push(item);
                        let item_num = item as usize;
                        alloc_assert!(!h.contains(&item_num));
                        h.insert(item_num);
                    }
                }

                for i in v {
                    unsafe {
                        my_alloc.free(i);
                    }
                }
            }));
        }
        for t in threads {
            t.join().expect("threads should exit successfully");
        }
    }
}
