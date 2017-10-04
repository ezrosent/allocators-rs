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
use super::slag::*;
use super::sources::MmapSource;
use super::utils::{likely, OwnedArray, LazyInitializable, mmap};
use super::alloc_type::AllocType;
use std::marker::PhantomData;
use std::mem;
use std::ptr;
use std::cmp;

pub trait Frontend: LazyInitializable + Clone {
    unsafe fn alloc(&mut self) -> *mut u8;
    unsafe fn free(&mut self, item: *mut u8);
}

/// A `LocalCache` provides thread-local data on top of a `SlagAllocator`.
///
/// Like a `MagazineCache`, it includes a stack of pointers to cache allocations. Unlike
/// `MagazineCache`, only pointers local to the current `Slag` are placed in this stack: the rest
/// are eagerly freed back to their owning `Slag`. Generally speaking, `LocalCache`s are better at
/// producer-consumer workloads and worse at predominantly thread-local workloads than the
/// `MagazineCache`. `LocalCache`s also have stronger guarantees when it comes to object locality:
/// almost all adjacent allocations will come from the same region of memory.
pub struct LocalCache<CA: CoarseAllocator> {
    alloc: SlagAllocator<CA>,
    vals: PtrStack,
    iter: AllocIter,
}

impl<CA: CoarseAllocator> Drop for LocalCache<CA> {
    fn drop(&mut self) {
        unsafe {
            let meta = &*self.alloc.m;
            let mask = self.iter.cur_word;
            let word = self.iter.next_word.offset(-1);
            let slag = self.alloc.slag;
            self.alloc.bulk_free(mask, word, slag, meta);
            for i in 0..self.vals.top {
                let item = *self.vals.data.get(i);
                self.alloc.free(item)
            }
        }
    }
}
impl<CA: CoarseAllocator> Clone for LocalCache<CA> {
    fn clone(&self) -> LocalCache<CA> {
        LocalCache::new(self.alloc.clone())
    }
}

impl<CA: CoarseAllocator> LocalCache<CA> {
    fn new(mut alloc: SlagAllocator<CA>) -> Self {
        unsafe {
            let stack = PtrStack::new((*alloc.m).n_objects);
            let iter = alloc.refresh();
            LocalCache {
                alloc: alloc,
                vals: stack,
                iter: iter,
            }
        }
    }
}

impl<CA: CoarseAllocator> Frontend for LocalCache<CA> {
    unsafe fn free(&mut self, it: *mut u8) {
        if self.alloc.contains(it) {
            self.vals.push(it);
        } else {
            self.alloc.free(it);
        }
    }

    unsafe fn alloc(&mut self) -> *mut u8 {
        self.vals
            .pop()
            .or_else(|| self.iter.next())
            .unwrap_or_else(|| {
                let next_iter = self.alloc.refresh();
                self.iter = next_iter;
                self.iter.next().expect("New iterator should have values")
            })
    }
}



/// A different approach to caching to `LocalCache` inspired by Bonwick-style magazines.
///
/// The advantage `MagazineCache` has over `LocalCache` is that it will unconditionally push to a
/// local data-structure if there is room (it doesn't have to be a local free). The magazine
/// structure also allows us to batch together remote frees (by simply composing bit-set masks
/// ahead of a fetch-or), reducing the number of atomic instruction that must be issued for most
/// remote frees.
pub struct MagazineCache<CA: CoarseAllocator> {
    stack_size: usize,
    s: PtrStack,
    iter: AllocIter,
    alloc: SlagAllocator<CA>,
    coalescer: Coalescer,
}

impl<CA: CoarseAllocator> LazyInitializable for MagazineCache<CA> {
    type Params = (*mut Metadata, usize, CA, RevocablePipe<Slag>);
    fn init(&(meta, decommit, ref page_alloc, ref avail): &Self::Params) -> Self {
        let salloc = SlagAllocator::partial_new(meta, decommit, page_alloc.clone(), avail.clone());
        Self::new(salloc)
    }
}

impl<CA: CoarseAllocator> LazyInitializable for LocalCache<CA> {
    type Params = (*mut Metadata, usize, CA, RevocablePipe<Slag>);
    fn init(&(meta, decommit, ref page_alloc, ref avail): &Self::Params) -> Self {
        let salloc = SlagAllocator::partial_new(meta, decommit, page_alloc.clone(), avail.clone());
        Self::new(salloc)
    }
}

impl<CA: CoarseAllocator> Drop for MagazineCache<CA> {
    fn drop(&mut self) {
        unsafe {
            let meta = &*self.alloc.m;
            let mask = self.iter.cur_word;
            let word = self.iter.next_word.offset(-1);
            let slag = self.alloc.slag;
            // bulk-free the current AllocIter word. Then free all elements in the magazine.
            self.alloc.bulk_free(mask, word, slag, meta);
            for i in 0..self.s.top {
                let item = *self.s.data.get(i);
                self.alloc.free(item)
            }
        }
    }
}

impl<CA: CoarseAllocator> Clone for MagazineCache<CA> {
    fn clone(&self) -> Self {
        MagazineCache::new_sized(self.alloc.clone(), self.stack_size)
    }
}

impl<CA: CoarseAllocator> MagazineCache<CA> {
    pub fn new_sized(mut alloc: SlagAllocator<CA>, magazine_size: usize) -> Self {
        assert!(magazine_size > 0);
        let s = PtrStack::new(magazine_size);
        let iter = unsafe { alloc.refresh() };
        let buckets = Coalescer::new(magazine_size * 2);
        MagazineCache {
            stack_size: magazine_size,
            s: s,
            iter: iter,
            alloc: alloc,
            coalescer: buckets,
        }
    }

    pub fn new(alloc: SlagAllocator<CA>) -> Self {
        use std::cmp;
        let object_size = unsafe { (*alloc.m).object_size };
        const CUTOFF: usize = 32 << 10;
        let magazine_size = match object_size {
            0...512 => 1 << 16,
            513...CUTOFF => 512 << 10 / object_size,
            _ => 1 << 20 / object_size,
        };
        Self::new_sized(alloc, cmp::max(1, magazine_size))
    }

    /// Allocate memory from the current owned `Slag`.
    ///
    /// This amounts to getting memory from the current alloc iterator. If the iterator is
    /// exhausted, a new `Slag` is acquired.
    unsafe fn slag_alloc(&mut self) -> *mut u8 {
        for _ in 0..2 {
            match self.iter.next() {
                Some(ptr) => return ptr,
                None => self.iter = self.alloc.refresh(),
            }
        }
        panic!(
            "New slag is empty {:?} {:?}",
            self.alloc.slag,
            (*self.alloc.slag).rc.load()
        )
    }

    /// Perform the bulk-level frees for the `Coalescer`.
    unsafe fn return_memory(&mut self) {
        debug_assert_eq!(self.s.top as usize, self.stack_size);
        let new_top = self.stack_size / 2;
        let meta = &*self.alloc.m;
        // iterate over the stack and attempt to add them to the coalescer.
        for i in new_top..self.stack_size {
            let item = *self.s.data.get(i);
            if !self.coalescer.insert(item, meta) {
                // there was a "hash collision", so we simply free `item` directly
                self.alloc.free(item)
            }
        }
        self.s.top = new_top;
        for cell_ptr in 0..self.coalescer.1.top {
            let cell = &mut **(self.coalescer.1.data.get(cell_ptr) as *mut *mut RemoteFreeCell);
            // Slag::find will technically work if you hand it any pointer within the slag
            // itself, not just an object. As a result, we use the reference count to get at
            // the slag it belongs to.
            let slag = Slag::find(cell.rc as *mut u8, meta.total_bytes);
            self.alloc.bulk_free(cell.mask, cell.word, slag, meta);
            ptr::write(cell, RemoteFreeCell::default());
        }
        self.coalescer.1.top = 0;
    }
}

impl<CA: CoarseAllocator> Frontend for MagazineCache<CA> {
    unsafe fn alloc(&mut self) -> *mut u8 {
        if let Some(ptr) = self.s.pop() {
            trace_event!(cache_alloc);
            ptr
        } else {
            trace_event!(slag_alloc);
            self.slag_alloc()
        }
    }

    unsafe fn free(&mut self, item: *mut u8) {
        trace_event!(local_free);
        if likely(self.s.top < self.stack_size) {
            self.s.push(item);
            return;
        }
        self.return_memory();
        self.s.push(item);
    }
}

/// A set data-structure used to batch remote free operations.
struct Coalescer(OwnedArray<RemoteFreeCell>, PtrStack);

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
            OwnedArray::new(size.next_power_of_two()),
            PtrStack::new(size),
        )
    }

    fn bucket_num(&self, word: usize) -> usize {
        // we can do a "fast mod" operation because we know len() is a power of two (see `new`)
        word & (self.0.len() - 1)
    }

    /// Try to insert `item`.
    ///
    /// The return value indicates if the value was successfully inserted.
    unsafe fn insert(&mut self, item: *mut u8, meta: &Metadata) -> bool {
        fn hash_ptr(p: *mut Word) -> usize {
            let p_num = p as usize;
            let words = p_num >> 3;
            let pages = words >> 18;
            pages.wrapping_mul(words)
        }
        let s = &*Slag::find(item, meta.total_bytes);
        let rc_ptr = &s.rc as *const _ as *mut RefCount;
        let (word, word_ix) = Slag::get_word(s.as_raw(), item, meta);
        let word_ptr = ((s.as_raw() as *mut u8).offset(meta.bitset_offset) as *mut Word)
            .offset(word);
        let bucket_ind = self.bucket_num(hash_ptr(word_ptr));
        let bucket = &mut *self.0.get(bucket_ind);
        // XXX: using the property of the creek implementation that it fills newly-dirtied pages
        // with zeros.
        if bucket.rc.is_null() {
            *bucket = RemoteFreeCell {
                rc: rc_ptr,
                word: word_ptr,
                mask: 1 << word_ix,
            };
            // append this to a stack of buckets we know are non-null
            self.1.push(bucket as *const _ as *mut u8);
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
    data: OwnedArray<*mut u8>,
    top: usize,
}

impl PtrStack {
    fn new(max_objects: usize) -> PtrStack {
        PtrStack {
            data: OwnedArray::new(max_objects),
            top: 0,
        }
    }

    unsafe fn push(&mut self, item: *mut u8) {
        *self.data.get(self.top) = item;
        self.top += 1;
    }

    unsafe fn pop(&mut self) -> Option<*mut u8> {
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
            debug_assert!(page_size.is_power_of_two());
            let bytes = mem::size_of::<Magazine>() + mem::size_of::<*mut u8>() * size;
            let rem = bytes & (page_size - 1);
            let n_pages = (bytes >> page_size.trailing_zeros()) + cmp::min(1, rem);
            let region_size = n_pages * page_size;
            debug_assert!(bytes <= region_size);
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
            debug_assert_eq!(((*slf).base as *mut Magazine).offset(-1), slf);
            debug_assert_eq!(slf as usize % mmap::page_size(), 0);
            mmap::unmap(slf as *mut u8, (*slf).mapped)
        }

        fn push(&mut self, item: *mut u8) -> bool {
            debug_assert!(!item.is_null());
            if self.top == self.cap {
                return false;
            }
            unsafe {
                let addr = (self.base as *mut *mut u8).offset(self.top as isize);
                debug_assert!((addr as isize - self.base as isize) < self.mapped as isize);
                ptr::write(addr, item);
            }
            self.top += 1;
            true
        }

        fn pop(&mut self) -> Option<*mut u8> {
            if self.top == 0 {
                return None;
            }
            unsafe {
                self.top -= 1;
                let res = ptr::read((self.base as *mut *mut u8).offset(self.top as isize));
                debug_assert!(!res.is_null());
                Some(res)
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

    impl<FE: Frontend> Drop for DepotCache<FE> {
        fn drop(&mut self) {
            for m in &[self.m1, self.m2] {
                let m_raw: *mut Magazine = *m;
                unsafe {
                    while let Some(p) = (*m_raw).pop() {
                        self.backing.free(p);
                    }
                    self.depot.free_empty(m_raw);
                }
            }
        }
    }

    impl Depot {
        fn new_size(max_size: usize, empty: usize, full: usize) -> Depot {
            assert!(max_size < (isize::max_value() as usize));
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
            debug_assert_eq!((*m).top, 0);
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
                debug_assert_eq!((*m).top, (*m).cap)
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
                    debug_assert_eq!((*r).top, (*r).cap)
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
                debug_assert_eq!((*res).top, 0)
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

    /// A magazine caching layer on top of a given `Frontend`.
    ///
    /// See module comments for more details on this algorithm.
    #[derive(Clone)]
    pub struct DepotCache<FE: Frontend> {
        backing: FE,
        depot: Depot,
        m1: *mut Magazine,
        m2: *mut Magazine,
    }

    impl<FE: Frontend> LazyInitializable for DepotCache<FE> {
        type Params = (FE::Params, Depot);
        fn init(&(ref backing, ref depot): &(FE::Params, Depot)) -> DepotCache<FE> {
            Self::new(FE::init(backing.clone()), depot.clone())
        }
    }

    impl<FE: Frontend> DepotCache<FE> {
        fn new(backing: FE, mut depot: Depot) -> DepotCache<FE> {
            let m1 = depot.alloc_full().unwrap_or_else(|| depot.alloc_empty());
            let m2 = depot.alloc_empty();
            DepotCache {
                backing: backing,
                depot: depot,
                m1: m1,
                m2: m2,
            }
        }
    }

    impl<FE: Frontend> Frontend for DepotCache<FE> {
        unsafe fn alloc(&mut self) -> *mut u8 {
            if let Some(p) = (*self.m1).pop() {
                return p;
            }
            mem::swap(&mut self.m1, &mut self.m2);
            if let Some(p) = (*self.m1).pop() {
                return p;
            }

            if let Some(m) = self.depot.swap_empty(self.m1) {
                self.m1 = m;
            } else {
                let cap = (*self.m1).cap;
                for _ in 0..cap {
                    let _r = (*self.m1).push(self.backing.alloc());
                    debug_assert!(_r);
                }
            }
            (*self.m1).pop().expect("new full magazine is empty")
        }

        unsafe fn free(&mut self, item: *mut u8) {
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
                        self.backing.free(x)
                    }
                },
            };
            let _r = (*self.m1).push(item);
            debug_assert!(_r);
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
                    assert!(m_ref.push(i as *mut u8));
                }
                assert!(!m_ref.push(8 as *mut u8));
                assert_eq!(m_ref.pop(), Some(m_ref.cap as *mut u8));
                assert!(m_ref.push(8 as *mut u8));
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
    pub fn build_local(&self) -> LocalAllocator<T> {
        LocalAllocator::new_standalone(
            self.cutoff_factor,
            self.page_size,
            self.target_overhead,
            self.eager_decommit_threshold,
            self.max_objects,
        )
    }

    /// Build a `MagazineAllocator<T>` from the current configuration.
    pub fn build_magazine(&self) -> MagazineAllocator<T> {
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
        pub struct $name<T>($wrapped<PageAlloc<MmapSource>>, PhantomData<T>);
        impl<T> Clone for $name<T> {
            fn clone(&self) -> Self {
                $name(self.0.clone(), PhantomData)
            }
        }

        impl<T> $name<T> {
            pub fn new_standalone(cutoff_factor: f64,
                                  page_size: usize,
                                  target_overhead: usize,
                                  eager_decommit: usize,
                                  max_objects: usize)
                -> Self {
                    let pa = PageAlloc::new(page_size, target_overhead, 8, AllocType::SmallSlag);
                    let slag = SlagAllocator::new(max_objects, mem::size_of::<T>(), 0,
                                                  cutoff_factor, eager_decommit, pa);
                    $name($wrapped::new(slag), PhantomData)
                }

            pub unsafe fn alloc(&mut self) -> *mut T {
                self.0.alloc() as *mut T
            }

            pub unsafe fn free(&mut self, item: *mut T) {
                self.0.free(item as *mut u8)
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
        assert!(mem::size_of::<T>() >= mem::size_of::<usize>());
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
                assert!(!h.contains(&item_num));
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
        assert!(mem::size_of::<T>() >= mem::size_of::<usize>());
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
                        assert!(!h.contains(&item_num));
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
