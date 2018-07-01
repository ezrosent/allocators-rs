// Copyright 2017-2018 the authors. See the 'Copyright and license' section of the
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
use std::{cmp, mem};
use std::intrinsics::likely;
use std::ptr::{self, NonNull};

use alloc_fmt::AllocUnwrap;

use backing::GCAlloc;
use slag::*;
use util::{as_ref, mmap, AllocWith, ConfigBuild, LazyGuard, MmapVec, PreDrop, TryCloneWith};

/// A type which can be used as an allocator frontend.
/// 
/// A `Frontend` must implement the following traits:
/// 
/// - `AllocWith` so that it can be allocated from
/// - `TryCloneWith` so that the top-level allocator handles can be `Clone`
///   or `TryClone`
/// - `PreDrop` because they do not store their own backing allocators, and
///   thus must have them provided by the caller in order to be able to free
///   any internally-cached resources
/// - `Send` so that the top-level allocator handles can be `Send`. Note that
///   they do *not* need to be `Sync` because, in general-purpose allocators,
///   they are wrapped in `Lazy`s which only require the configuration type
///   (`ConfigBuild::Config`) to be `Sync` in order for the `Lazy` to be `Sync`.
/// - `ConfigBuild` where the `Config` type is `SlagAllocator`'s `Config` type
///   as frontends wrap `SlagAllocator`s. Doing it this way (rather than
///   requiring a `new` method with a `SlagAllocator` parameter) covers both
///   the case of constructing a new `Frontend` immediately and also of creating
///   a `Lazy`.
pub trait Frontend<B>
where
    Self: AllocWith<B> + TryCloneWith<B> + PreDrop<B> + Sized + Send,
    Self: ConfigBuild<B, Config = <SlagAllocator as ConfigBuild<B>>::Config>,
    B: GCAlloc, // Required by the ConfigBuild impl for SlagAllocator
{}

impl<T, B> Frontend<B> for T
where
    Self: AllocWith<B> + TryCloneWith<B> + PreDrop<B> + Sized + Send,
    Self: ConfigBuild<B, Config = <SlagAllocator as ConfigBuild<B>>::Config>,
    B: GCAlloc,
{}

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

impl LocalCache {
    fn new<B: GCAlloc>(guard: &LazyGuard, backing: &mut B, mut alloc: SlagAllocator) -> Option<Self> {
        unsafe {
            let stack = PtrStack::new((*alloc.m.as_ptr()).n_objects)?;
            let iter = alloc.refresh(guard, backing)?;
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

impl<B: GCAlloc> ConfigBuild<B> for LocalCache {
    type Config = <SlagAllocator as ConfigBuild<B>>::Config;
    fn build(cfg: &Self::Config, guard: &LazyGuard, backing: &mut B) -> Option<Self> {
        let alloc = SlagAllocator::build(cfg, guard, backing)?;
        Some(Self::new(guard, backing, alloc)?)
    }
}

unsafe impl<B: GCAlloc> AllocWith<B> for LocalCache {
    unsafe fn alloc_with(&mut self, guard: &LazyGuard, backing: &mut B) -> Option<NonNull<u8>> {
        self.vals
            .pop()
            .or_else(|| self.iter.next())
            .or_else(|| {
                self.iter = self.alloc.refresh(guard, backing)?;
                Some(self.iter.next().alloc_expect("new iterator should have values"))
            })
    }

    unsafe fn dealloc_with(&mut self, guard: &LazyGuard, backing: &mut B, it: NonNull<u8>) {
        if self.alloc.contains(it) {
            self.vals.push(it);
        } else {
            self.alloc.dealloc(guard, backing, it);
        }
    }
}

unsafe impl<B: GCAlloc> PreDrop<B> for LocalCache {
    /// Free all resources in preparation for dropping `self`.
    /// 
    /// Since freeing the resources in a `LocalCache` requires both a Crossbeam
    /// epoch GC guard and a page allocator, neither of which are stored
    /// internally, the freeing logic cannot go in `Drop::drop`, and thus the
    /// owner of this `LocalCache` is responsible for calling `pre_drop` before
    /// dropping it.
    unsafe fn pre_drop(&mut self, guard: &LazyGuard, backing: &mut B) {
        #[cfg(debug_assertions)]
        {
            alloc_debug_assert!(!self.dropped);
            self.dropped = true;
        }
        let meta = as_ref(self.alloc.m);
        let mask = self.iter.cur_word;
        let word = NonNull::new_unchecked(self.iter.next_word.offset(-1));
        let slag = self.alloc.slag;
        self.alloc.bulk_dealloc(guard, backing, mask, word, slag, meta);
        for i in 0..self.vals.top {
            let item = *self.vals.data.get_mut_debug_checked(i);
            self.alloc.dealloc(guard, backing, item)
        }
        self.alloc.pre_drop(guard, backing);
    }
}

#[cfg(debug_assertions)]
impl Drop for LocalCache {
    fn drop(&mut self) {
        alloc_debug_assert!(self.dropped);
    }
}

impl<B: GCAlloc> TryCloneWith<B> for LocalCache {
    fn try_clone_with(&self, guard: &LazyGuard, backing: &mut B) -> Option<Self> {
        // TODO: Avoid leaking resources on failure (see issue #179)
        let alloc = self.alloc.try_clone_with(guard, backing)?;
        LocalCache::new(guard, backing, alloc)
    }
}

unsafe impl Send for LocalCache {}

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

impl<B: GCAlloc> ConfigBuild<B> for MagazineCache {
    type Config = <SlagAllocator as ConfigBuild<B>>::Config;
    fn build(cfg: &Self::Config, guard: &LazyGuard, backing: &mut B) -> Option<Self> {
        let alloc = SlagAllocator::build(cfg, guard, backing)?;
        Some(Self::new(guard, backing, alloc)?)
    }
}

impl MagazineCache {
    pub fn new_sized<B: GCAlloc>(guard: &LazyGuard, backing: &mut B, mut alloc: SlagAllocator, magazine_size: usize) -> Option<Self> {
        alloc_assert!(magazine_size > 0);
        let s = PtrStack::new(magazine_size)?;
        let iter = unsafe { alloc.refresh(guard, backing)? };
        let buckets = Coalescer::new(magazine_size * 2)?;
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

    fn new<B: GCAlloc>(guard: &LazyGuard, backing: &mut B, alloc: SlagAllocator) -> Option<Self> {
        use std::cmp;
        let object_size = unsafe { alloc.m.as_ref().object_size };
        const CUTOFF: usize = 32 << 10;
        let magazine_size = match object_size {
            0...512 => 1 << 16,
            513...CUTOFF => 512 << 10 / object_size,
            _ => 1 << 20 / object_size,
        };
        Self::new_sized(guard, backing, alloc, cmp::max(1, magazine_size))
    }

    /// Allocate memory from the current owned `Slag`.
    ///
    /// This amounts to getting memory from the current alloc iterator. If the iterator is
    /// exhausted, a new `Slag` is acquired.
    unsafe fn slag_alloc<B: GCAlloc>(&mut self, guard: &LazyGuard, backing: &mut B) -> Option<NonNull<u8>> {
        for _ in 0..2 {
            match self.iter.next() {
                Some(ptr) => return Some(ptr),
                None => self.iter = self.alloc.refresh(guard, backing)?,
            }
        }
        alloc_panic!(
            "New slag is empty {:?} {:?}",
            self.alloc.slag,
            self.alloc.slag.as_ref().rc.load()
        )
    }

    /// Perform the bulk-level frees for the `Coalescer`.
    unsafe fn return_memory<B: GCAlloc>(&mut self, guard: &LazyGuard, backing: &mut B) {
        alloc_debug_assert_eq!(self.s.top as usize, self.stack_size);
        let new_top = self.stack_size / 2;
        let meta = as_ref(self.alloc.m);
        // iterate over the stack and attempt to add them to the coalescer.
        for i in new_top..self.stack_size {
            let item = *self.s.data.get_mut_debug_checked(i);
            if !self.coalescer.insert(item, meta) {
                // there was a "hash collision", so we simply free `item` directly
                self.alloc.dealloc(guard, backing, item)
            }
        }
        self.s.top = new_top;
        for cell_ptr in 0..self.coalescer.1.top {
            let cell = &mut **(self.coalescer.1.data.get_mut_debug_checked(cell_ptr) as *mut *mut RemoteFreeCell);
            // Slag::find will technically work if you hand it any pointer within the slag
            // itself, not just an object. As a result, we use the reference count to get at
            // the slag it belongs to.
            let slag = Slag::find(NonNull::new_unchecked(cell.rc as *mut _), meta.total_bytes);
            let word = NonNull::new_unchecked(cell.word);
            self.alloc.bulk_dealloc(guard, backing, cell.mask, word, slag, meta);
            ptr::write(cell, RemoteFreeCell::default());
        }
        self.coalescer.1.top = 0;
    }
}

unsafe impl<B: GCAlloc> AllocWith<B> for MagazineCache {
    unsafe fn alloc_with(&mut self, guard: &LazyGuard, backing: &mut B) -> Option<NonNull<u8>> {
        if let Some(ptr) = self.s.pop() {
            trace_event!(cache_alloc);
            Some(ptr)
        } else {
            trace_event!(slag_alloc);
            self.slag_alloc(guard, backing)
        }
    }

    unsafe fn dealloc_with(&mut self, guard: &LazyGuard, backing: &mut B, item: NonNull<u8>) {
        trace_event!(local_free);
        if likely(self.s.top < self.stack_size) {
            self.s.push(item);
            return;
        }
        self.return_memory(guard, backing);
        self.s.push(item);
    }
}

unsafe impl<B: GCAlloc> PreDrop<B> for MagazineCache {
    /// Free all resources in preparation for dropping `self`.
    /// 
    /// Since freeing the resources in a `MagazineCache` requires both a
    /// Crossbeam epoch GC guard and a page allocator, neither of which are
    /// stored internally, the freeing logic cannot go in `Drop::drop`, and
    /// thus the owner of this `MagazineCache` is responsible for calling
    /// `pre_drop` before dropping it.
    unsafe fn pre_drop(&mut self, guard: &LazyGuard, backing: &mut B) {
        #[cfg(debug_assertions)]
        {
            alloc_debug_assert!(!self.dropped);
            self.dropped = true;
        }
        let meta = as_ref(self.alloc.m);
        let mask = self.iter.cur_word;
        let word = NonNull::new_unchecked(self.iter.next_word.offset(-1));
        let slag = self.alloc.slag;
        // bulk-free the current AllocIter word. Then free all elements in the magazine.
        self.alloc.bulk_dealloc(guard, backing, mask, word, slag, meta);
        for i in 0..self.s.top {
            let item = *self.s.data.get_mut_debug_checked(i);
            self.alloc.dealloc(guard, backing, item)
        }
        self.alloc.pre_drop(guard, backing);
    }
}

#[cfg(debug_assertions)]
impl Drop for MagazineCache {
    fn drop(&mut self) {
        alloc_debug_assert!(self.dropped)
    }
}

impl<B: GCAlloc> TryCloneWith<B> for MagazineCache {
    fn try_clone_with(&self, guard: &LazyGuard, backing: &mut B) -> Option<Self> {
        // TODO: Avoid leaking resources on failure (see issue #179)
        let alloc = self.alloc.try_clone_with(guard, backing)?;
        MagazineCache::new_sized(guard, backing, alloc, self.stack_size)
    }
}

unsafe impl Send for MagazineCache {}

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
    fn new(size: usize) -> Option<Self> {
        Some(Coalescer(
            MmapVec::new(size.next_power_of_two())?,
            PtrStack::new(size)?,
        ))
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
        let bucket = &mut *self.0.get_mut_debug_checked(bucket_ind);
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
    fn new(max_objects: usize) -> Option<PtrStack> {
        Some(PtrStack {
            data: MmapVec::new(max_objects)?,
            top: 0,
        })
    }

    unsafe fn push(&mut self, item: NonNull<u8>) {
        *self.data.get_mut_debug_checked(self.top) = item;
        self.top += 1;
    }

    unsafe fn pop(&mut self) -> Option<NonNull<u8>> {
        if self.empty() {
            None
        } else {
            self.top -= 1;
            Some(*self.data.get_mut_debug_checked(self.top))
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
    use bagpipe::{BagCleanup, BagPipe};
    use bagpipe::bag::WeakBag;
    use bagpipe::queue::FAAQueueLowLevel;

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
            let mem = mmap::map(region_size).cast().as_ptr();
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
            mmap::unmap(NonNull::new_unchecked(slf).cast(), (*slf).mapped)
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
        unsafe fn free_empty(&mut self, guard: &LazyGuard, m: *mut Magazine) {
            alloc_debug_assert_eq!((*m).top, 0);
            if self.empty.size_guess() >= self.max_size {
                Magazine::destroy(m);
            } else {
                self.empty.push_mut(&guard.guard(), m)
            }
        }

        /// Return a full `Magazine` to the `Depot`.
        ///
        /// If the `Depot` is at capacity, the `Magazine` is not added to the `Depot` and this
        /// method returns `false`.
        fn free_full(&mut self, guard: &LazyGuard, m: *mut Magazine) -> bool {
            unsafe {
                alloc_debug_assert_eq!((*m).top, (*m).cap)
            };
            if self.full.size_guess() >= self.max_size {
                false
            } else {
                self.full.push_mut(&guard.guard(), m);
                true
            }
        }

        /// Allocate a full `Magazine` from the `Depot` if one is present.
        fn alloc_full(&mut self, guard: &LazyGuard) -> Option<*mut Magazine> {
            self.full.pop_mut(&guard.guard()).and_then(|r| {
                unsafe {
                    alloc_debug_assert_eq!((*r).top, (*r).cap)
                };
                Some(r)
            })
        }

        /// Allocate a full `Magazine` from the `Depot`, constructing a new one if none are
        /// present.
        fn alloc_empty(&mut self, guard: &LazyGuard) -> *mut Magazine {
            let res = self.empty.pop_mut(&guard.guard()).unwrap_or_else(|| {
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
        fn swap_empty(&mut self, guard: &LazyGuard, m: *mut Magazine) -> Option<*mut Magazine> {
            self.alloc_full(guard).and_then(|new_m| {
                unsafe {
                    self.free_empty(guard, m)
                };
                Some(new_m)
            })
        }

        /// Swap out a full `Magazine` for an empty one.
        ///
        /// If the `Depot` is at capacity, `None` is returned and `m` is not freed.
        fn swap_full(&mut self, guard: &LazyGuard, m: *mut Magazine) -> Option<*mut Magazine> {
            if self.free_full(guard, m) {
                Some(self.alloc_empty(guard))
            } else {
                None
            }
        }
    }

    /// A magazine caching layer on top of a given frontend.
    ///
    /// See module comments for more details on this algorithm.
    #[derive(Clone)]
    pub struct DepotCache<A> {
        backing: A,
        depot: Depot,
        m1: *mut Magazine,
        m2: *mut Magazine,
        #[cfg(debug_assertions)]
        dropped: bool,
    }

    impl<A, B> ConfigBuild<B> for DepotCache<A>
    where
        A: AllocWith<B> + ConfigBuild<B>,
    {
        type Config = (A::Config, Depot);
        fn build(&(ref a_cfg, ref depot): &Self::Config, guard: &LazyGuard, backing: &mut B) -> Option<DepotCache<A>> {
            Some(Self::new(guard, A::build(a_cfg, guard, backing)?, depot.clone()))
        }
    }

    impl<A> DepotCache<A> {
        fn new(guard: &LazyGuard, backing: A, mut depot: Depot) -> DepotCache<A> {
            let m1 = depot.alloc_full(guard).unwrap_or_else(|| depot.alloc_empty(guard));
            let m2 = depot.alloc_empty(guard);
            DepotCache {
                backing,
                depot,
                m1,
                m2,
                #[cfg(debug_assertions)]
                dropped: false,
            }
        }
    }

    unsafe impl<A, B> AllocWith<B> for DepotCache<A>
    where
        A: AllocWith<B>,
    {
        unsafe fn alloc_with(&mut self, guard: &LazyGuard, backing: &mut B) -> Option<NonNull<u8>> {
            if let Some(p) = (*self.m1).pop() {
                return Some(p);
            }
            mem::swap(&mut self.m1, &mut self.m2);
            if let Some(p) = (*self.m1).pop() {
                return Some(p);
            }

            if let Some(m) = self.depot.swap_empty(guard, self.m1) {
                self.m1 = m;
            } else {
                let cap = (*self.m1).cap;
                for _ in 0..cap {
                    let _r = (*self.m1).push(self.backing.alloc_with(guard, backing)?);
                    alloc_debug_assert!(_r);
                }
            }
            Some((*self.m1).pop().alloc_expect("new full magazine is empty"))
        }

        unsafe fn dealloc_with(&mut self, guard: &LazyGuard, backing: &mut B, item: NonNull<u8>) {
            if (*self.m1).push(item) {
                return;
            }
            mem::swap(&mut self.m1, &mut self.m2);

            if (*self.m1).push(item) {
                return;
            }
            match self.depot.swap_full(guard, self.m1) {
                Some(m) => self.m1 = m,
                None => {
                    while let Some(x) = (*self.m1).pop() {
                        self.backing.dealloc_with(guard, backing, x)
                    }
                },
            };
            let _r = (*self.m1).push(item);
            alloc_debug_assert!(_r);
        }
    }

    unsafe impl<A, B> PreDrop<B> for DepotCache<A>
    where
        A: AllocWith<B> + PreDrop<B>,
    {
        unsafe fn pre_drop(&mut self, guard: &LazyGuard, backing: &mut B) {
            #[cfg(debug_assertions)]
            {
                alloc_debug_assert!(self.dropped);
                self.dropped = true;
            }
            for m in &[self.m1, self.m2] {
                let m_raw: *mut Magazine = *m;
                while let Some(p) = (*m_raw).pop() {
                    self.backing.dealloc_with(guard, backing, p);
                }
                self.depot.free_empty(guard, m_raw);
            }
            self.backing.pre_drop(guard, backing);
        }
    }

    #[cfg(debug_assertions)]
    impl<A> Drop for DepotCache<A> {
        fn drop(&mut self) {
            alloc_debug_assert!(self.dropped);
        }
    }

    #[cfg(test)]
    mod tests {
        use alloc_fmt::AllocUnwrap;

        use super::*;

        #[test]
        fn magazine_stack() {
            unsafe {
                let m = Magazine::default();
                let m_ref = &mut*m;
                let goal = m_ref.cap;
                for i in 1..(goal + 1) {
                    alloc_assert!(m_ref.push(NonNull::new_unchecked(i as *mut _)));
                }
                alloc_assert!(!m_ref.push(NonNull::new_unchecked(8 as *mut _)));
                alloc_assert_eq!(m_ref.pop().alloc_unwrap().as_ptr(), m_ref.cap as *mut _);
                alloc_assert!(m_ref.push(NonNull::new_unchecked(8 as *mut _)));
                Magazine::destroy(m);
            }
        }
    }
}
