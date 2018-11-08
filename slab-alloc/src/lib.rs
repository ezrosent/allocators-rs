// Copyright 2017-2018 the authors. See the 'Copyright and license' section of the
// README.md file at the top-level directory of this repository.
//
// Licensed under the Apache License, Version 2.0 (the LICENSE-APACHE file) or
// the MIT license (the LICENSE-MIT file) at your option. This file may not be
// copied, modified, or distributed except according to those terms.

// So clippy doesn't complain that SunOS isn't in tick marks
#![cfg_attr(feature = "cargo-clippy", allow(doc_markdown))]
//! An `ObjectAlloc` which allocates objects in contiguous slabs and caches constructed objects.
//!
//! # Design
//!
//! The `SlabAlloc` is based largely on the slab allocator design originally inroduced in the
//! SunOS 5.4 kernel and described in depth in [The Slab Allocator: An Object-Caching Kernel Memory
//! Allocator][1]. This implementation stays somewhat true to the original design, although it
//! includes a number of performance improvements and modifications for user-land.
//!
//! The `SlabAlloc` provides a number of performance benefits:
//!
//! * As with any `ObjectAlloc`, object caching reduces the overhead of object initialization in
//!   many cases.
//! * As with any `ObjectAlloc`, only needing to allocate objects of a particular size and
//!   alignment allows for optimizations not available to general-purpose allocators.
//! * Internal and external fragmentation are kept to a minimum.
//! * A "coloring" scheme described in Section 4 of the original paper improves cache utilization.
//!
//! [1]: http://www.usenix.org/publications/library/proceedings/bos94/full_papers/bonwick.ps

// TODO:
// - Sort partially-full slabs so that the emptiest slabs come first. This makes it more likely
//   that almost-full slabs will become completely full and eligible to be freed back to the system.
//   NOTE: Consider using a simplified heap like the one used in SuperMalloc.
// - Find a fitting algorithm slabs that trades off space usage with ability to perform coloring
//   (rather than always prioritizing space usage)
// - Would it be worth it to special-case 64-byte allocations to ensure that they are 64-byte
//   aligned so that each object gets its own cache line? It should be sufficient to artificially
//   override the align parameter to be 64 bytes when the size is 64 bytes.
// - Add a feature flag to disable using aligned slabs for backing sizes larger than a page (to
//   enable benchmarking aligned vs large slabs)
// - Consider improvements to the slab size selection algorithm. For example, might we be willing
//   to take sub-optimal space utilization in order to use an aligned slab with fewer than the
//   number of objects per slab, and thus reap the performance benefits of aligned slabs?
// - Make sure everything is exception-safe.

// Guarantees made by Rust about the memory layout (see https://doc.rust-lang.org/nomicon/repr-rust.html):
// - Addresses of type T must be a multiple of T's alignment
// - All alignments must be powers of two
// - T's size must be a multiple of its alignment
//   - Corrollary: given an array of T, the offset of the ith element is just i * sizeof(T)

#![cfg_attr(not(feature = "std"), no_std)]
#![feature(alloc, allocator_api)]
#![cfg_attr(test, feature(test))]

mod aligned;
mod backing;
mod init;
mod large;
mod ptr_map;
mod stack;
#[cfg(test)]
mod tests;
mod util;

extern crate alloc;
#[cfg(feature = "std")]
extern crate core;
#[macro_use]
extern crate lazy_static;
extern crate object_alloc;
#[cfg(test)]
#[macro_use]
extern crate object_alloc_test;
extern crate sysconf;
#[cfg(test)]
extern crate test;

use self::alloc::alloc::Layout;
use self::init::InitSystem;
use self::object_alloc::{ObjectAlloc, UntypedObjectAlloc};
use self::util::list::*;
use core::default::Default;
use core::marker::PhantomData;
use core::mem;
use core::ptr::NonNull;
use init::*;
use util::workingset::WorkingSet;

#[cfg(feature = "std")]
use backing::heap::HeapBackingAlloc;
#[cfg(feature = "os")]
use backing::mmap::MmapBackingAlloc;
pub use backing::BackingAlloc;

use init::NopInitSystem;
type DefaultInitSystem<T> = init::InitInitSystem<T, init::DefaultInitializer<T>>;
type FnInitSystem<T, F> = init::InitInitSystem<T, init::FnInitializer<T, F>>;
type UnsafeFnInitSystem<T, F> = init::InitInitSystem<T, init::UnsafeFnInitializer<T, F>>;

lazy_static! {
    static ref PAGE_SIZE: usize = self::sysconf::page::pagesize();
    static ref PAGE_ALIGN_MASK: usize = !(*PAGE_SIZE - 1);
}

const WORKING_PERIOD_SECONDS: u64 = 15;
const OBJECTS_PER_SLAB: usize = 8;

/// A typed slab allocator.
pub struct SlabAlloc<T, I: InitSystem, B: BackingAlloc> {
    alloc: PrivateSlabAlloc<I, B>,
    _marker: PhantomData<T>,
}

/// An untyped slab allocator.
pub struct UntypedSlabAlloc<I: InitSystem, B: BackingAlloc> {
    alloc: PrivateUntypedSlabAlloc<I, B>,
}

enum PrivateSlabAlloc<I: InitSystem, B: BackingAlloc> {
    Aligned(SizedSlabAlloc<I, aligned::System<B::Aligned>>),
    Large(SizedSlabAlloc<I, large::System<B::Large>>),
}

enum PrivateUntypedSlabAlloc<I: InitSystem, B: BackingAlloc> {
    Aligned(SizedSlabAlloc<I, aligned::System<B::Aligned>>),
    Large(SizedSlabAlloc<I, large::System<B::Large>>),
}

/// A builder for `SlabAlloc`s.
pub struct SlabAllocBuilder<T, I: InitSystem> {
    init: I,
    layout: Layout,
    _marker: PhantomData<T>,
}

impl<T, I: InitSystem> SlabAllocBuilder<T, I> {
    /// Updates the alignment guaranteed by the allocator.
    ///
    /// `align` must not be greater than the size of `T` (that is, `core::mem::size_of::<T>()`),
    /// must not be greater than the system's page size, and must be a power of two. The size of
    /// `T` must be a multiple of `align`.
    ///
    /// If `align` is called multiple times, then the largest specified alignment will be used.
    /// Since all alignments must be powers of two, allocations which satisfy the largest specified
    /// alignment will also satisfy all smaller alignments.
    pub fn align(mut self, align: usize) -> SlabAllocBuilder<T, I> {
        assert!(align.is_power_of_two());
        assert!(align <= mem::size_of::<T>());
        assert_eq!(mem::size_of::<T>() % align, 0);
        assert!(align <= *PAGE_SIZE);
        self.layout = self.layout.align_to(align);
        self
    }

    /// Builds a `SlabAlloc` whose memory is backed by the heap.
    #[cfg(feature = "std")]
    pub fn build(self) -> SlabAlloc<T, I, HeapBackingAlloc> {
        use backing::heap::{get_aligned, get_large};
        self.build_backing(get_aligned, get_large)
    }

    /// Builds a `SlabAlloc` whose memory is backed by mmap.
    ///
    /// On Unix and Linux, `mmap` is used. On Windows, `VirtualAlloc` is used.
    #[cfg(feature = "os")]
    pub fn build_mmap(self) -> SlabAlloc<T, I, MmapBackingAlloc> {
        use backing::mmap::{get_aligned, get_large};
        self.build_backing(get_aligned, get_large)
    }

    /// Builds an `UntypedSlabAlloc` whose memory is backed by the heap.
    #[cfg(feature = "std")]
    pub fn build_untyped(self) -> UntypedSlabAlloc<I, HeapBackingAlloc> {
        use backing::heap::{get_aligned, get_large};
        self.build_untyped_backing(get_aligned, get_large)
    }

    /// Builds an `UntypedSlabAlloc` whose memory is backed by mmap.
    ///
    /// On Unix and Linux, `mmap` is used. On Windows, `VirtualAlloc` is used.
    #[cfg(feature = "os")]
    pub fn build_untyped_mmap(self) -> UntypedSlabAlloc<I, MmapBackingAlloc> {
        use backing::mmap::{get_aligned, get_large};
        self.build_untyped_backing(get_aligned, get_large)
    }

    /// Builds a new `SlabAlloc` with a custom memory provider.
    ///
    /// `build_backing` builds a new `SlabAlloc` from the configuration `self`. `SlabAlloc`s get
    /// their memory from `UntypedObjectAlloc`s which allocate the memory necessary to back a
    /// single slab. Under the hood, slab allocators use two types of slabs - "aligned" slabs and
    /// "large" slabs. Aligned slabs perform better, but are not always supported for all slab
    /// sizes. Large slabs do not perform as well, but are supported for all slab sizes. In
    /// particular, aligned slabs require that the memory used to back them is aligned to its own
    /// size (ie, a 16K slab has alignment 16K, etc), while large slabs only require page alignment
    /// regardless of the slab size. All slabs are always at least one page in size and are always
    /// page-aligned at a minimum.
    ///
    /// Because support for a particular slab size may not be knowable until runtime (e.g., it may
    /// depend on the page size, which can vary by system), which slab type will be used cannot be
    /// known at compile time. Instead, `build_backing` computes the ideal aligned slab size, and
    /// calls `get_aligned` with a `Layout` describing that size. `get_aligned` returns an `Option`
    ///  - `None` if the requested size is not supported, and `Some` if the size is supported. If
    /// the size is not supported, `build_backing` will fall back to using large slabs, and will
    /// call `get_large` to get an allocator. It will only call `get_large` with a `Layout` that is
    /// required to be supported (at least a page in size and page-aligned), so `get_large` returns
    /// an allocator directly rather than an `Option`.
    pub fn build_backing<B, A, L>(self, get_aligned: A, get_large: L) -> SlabAlloc<T, I, B>
    where
        B: BackingAlloc,
        A: Fn(Layout) -> Option<B::Aligned>,
        L: Fn(Layout) -> B::Large,
    {
        let layout = util::misc::satisfy_min_align(self.layout.clone(), I::min_align());
        let aligned_backing_size = aligned::backing_size_for::<I>(&layout);
        let aligned_slab_layout =
            Layout::from_size_align(aligned_backing_size, aligned_backing_size).unwrap();
        SlabAlloc {
            alloc: if let Some(alloc) = get_aligned(aligned_slab_layout) {
                let data = aligned::System::new(layout, alloc).unwrap();
                PrivateSlabAlloc::Aligned(SizedSlabAlloc::new(self.init, self.layout, data))
            } else {
                let backing_size = large::backing_size_for::<I>(&layout);
                let slab_layout = Layout::from_size_align(backing_size, *PAGE_SIZE).unwrap();
                let data = large::System::new(layout, get_large(slab_layout)).unwrap();
                PrivateSlabAlloc::Large(SizedSlabAlloc::new(self.init, self.layout, data))
            },
            _marker: PhantomData,
        }
    }

    /// Builds a new `UntypedSlabAlloc` with a custom memory provider.
    ///
    /// `build_untyped_backing` is like `build_backing`, except that it builds an
    /// `UntypedSlabAlloc` instead of a `SlabAlloc`.
    pub fn build_untyped_backing<B, A, L>(
        self,
        get_aligned: A,
        get_large: L,
    ) -> UntypedSlabAlloc<I, B>
    where
        B: BackingAlloc,
        A: Fn(Layout) -> Option<B::Aligned>,
        L: Fn(Layout) -> B::Large,
    {
        let layout = util::misc::satisfy_min_align(self.layout.clone(), I::min_align());
        let aligned_backing_size = aligned::backing_size_for::<I>(&layout);
        let aligned_slab_layout =
            Layout::from_size_align(aligned_backing_size, aligned_backing_size).unwrap();
        UntypedSlabAlloc {
            alloc: if let Some(alloc) = get_aligned(aligned_slab_layout) {
                let data = aligned::System::new(layout, alloc).unwrap();
                PrivateUntypedSlabAlloc::Aligned(SizedSlabAlloc::new(self.init, self.layout, data))
            } else {
                let backing_size = large::backing_size_for::<I>(&layout);
                let slab_layout = Layout::from_size_align(backing_size, *PAGE_SIZE).unwrap();
                let data = large::System::new(layout, get_large(slab_layout)).unwrap();
                PrivateUntypedSlabAlloc::Large(SizedSlabAlloc::new(self.init, self.layout, data))
            },
        }
    }
}

impl<T: Default> SlabAllocBuilder<T, DefaultInitSystem<T>> {
    /// Constructs a new builder for an allocator which uses `T::default` to initialize allocated
    /// objects.
    ///
    /// The constructed allocator will call `T::default` whenever a new object needs to be
    /// initialized.
    pub fn default() -> SlabAllocBuilder<T, DefaultInitSystem<T>> {
        SlabAllocBuilder {
            init: DefaultInitSystem::new(DefaultInitializer::new()),
            layout: Layout::new::<T>(),
            _marker: PhantomData,
        }
    }
}

impl<T, F: Fn() -> T> SlabAllocBuilder<T, FnInitSystem<T, F>> {
    /// Constructs a new builder for an allocator which uses `f` to initialize allocated objects.
    ///
    /// The constructed allocator will call `f` whenever a new object needs to be initialized.
    pub fn func(f: F) -> SlabAllocBuilder<T, FnInitSystem<T, F>> {
        SlabAllocBuilder {
            init: FnInitSystem::new(FnInitializer::new(f)),
            layout: Layout::new::<T>(),
            _marker: PhantomData,
        }
    }
}

impl<T, F: Fn(NonNull<T>)> SlabAllocBuilder<T, UnsafeFnInitSystem<T, F>> {
    /// Constructs a new builder for an allocator which uses `f` to initialize allocated objects.
    ///
    /// The constructed allocator will call `f` whenever a new object needs to be initialized.
    /// A pointer to the uninitialized memory will be passed, and it is `f`'s responsibility to
    /// initialize this memory to a valid instance of `T`.
    ///
    /// # Safety
    /// This function is `unsafe` because passing a function which does not abide by the documented
    /// contract could result in an allocator handing out uninitialized or invalid memory.
    pub unsafe fn unsafe_func(f: F) -> SlabAllocBuilder<T, UnsafeFnInitSystem<T, F>> {
        SlabAllocBuilder {
            init: UnsafeFnInitSystem::new(UnsafeFnInitializer::new(f)),
            layout: Layout::new::<T>(),
            _marker: PhantomData,
        }
    }
}

impl<T> SlabAllocBuilder<T, NopInitSystem> {
    /// Constructs a new builder for an allocator which does not initialize allocated objects.
    /// Objects returned by `alloc` are not guaranteed to be valid instances of `T`.
    ///
    /// # Safety
    /// This function is `unsafe` because it constructs an allocator which will hand out
    /// uninitialized memory.
    pub unsafe fn no_initialize() -> SlabAllocBuilder<T, NopInitSystem> {
        SlabAllocBuilder {
            init: NopInitSystem,
            layout: Layout::new::<T>(),
            _marker: PhantomData,
        }
    }
}

/// A builder for `UntypedSlabAlloc`s.
pub struct UntypedSlabAllocBuilder<I: InitSystem> {
    init: I,
    layout: Layout,
}

impl<I: InitSystem> UntypedSlabAllocBuilder<I> {
    /// Updates the alignment guaranteed by the allocator.
    ///
    /// `align` must not be greater than the size of allocated objects, must not be greater than
    /// the system's page size, and must be a power of two. The size of allocated objects must be a
    /// multiple of `align`.
    ///
    /// If `align` is called multiple times, then the largest specified alignment will be used.
    /// Since all alignments must be powers of two, allocations which satisfy the largest specified
    /// alignment will also satisfy all smaller alignments.
    pub fn align(mut self, align: usize) -> UntypedSlabAllocBuilder<I> {
        assert!(align.is_power_of_two());
        assert!(align <= self.layout.size());
        assert_eq!(self.layout.size() % align, 0);
        assert!(align <= *PAGE_SIZE);
        self.layout = self.layout.align_to(align);
        self
    }

    /// Builds an `UntypedSlabAlloc` whose memory is backed by the heap.
    #[cfg(feature = "std")]
    pub fn build(self) -> UntypedSlabAlloc<I, HeapBackingAlloc> {
        use backing::heap::{get_aligned, get_large};
        self.build_backing(get_aligned, get_large)
    }

    /// Builds an `UntypedSlabAlloc` whose memory is backed by mmap.
    ///
    /// On Unix and Linux, `mmap` is used. On Windows, `VirtualAlloc` is used.
    #[cfg(feature = "os")]
    pub fn build_mmap(self) -> UntypedSlabAlloc<I, MmapBackingAlloc> {
        use backing::mmap::{get_aligned, get_large};
        self.build_backing(get_aligned, get_large)
    }

    /// Builds a new `UntypedSlabAlloc` with a custom memory provider.
    ///
    /// `build_backing` builds a new `UntypedSlabAlloc` from the configuration `self`.
    /// `UntypedSlabAlloc`s get their memory from `UntypedObjectAlloc`s which allocate the memory
    /// necessary to back a single slab. Under the hood, slab allocators use two types of slabs -
    /// "aligned" slabs and "large" slabs. Aligned slabs perform better, but are not always
    /// supported for all slab sizes. Large slabs do not perform as well, but are supported for all
    /// slab sizes. In particular, aligned slabs require that the memory used to back them is
    /// aligned to its own size (ie, a 16K slab has alignment 16K, etc), while large slabs only
    /// require page alignment regardless of the slab size. All slabs are always at least one page
    /// in size and are always page-aligned at a minimum.
    ///
    /// Because support for a particular slab size may not be knowable until runtime (e.g., it may
    /// depend on the page size, which can vary by system), which slab type will be used cannot be
    /// known at compile time. Instead, `build_backing` computes the ideal aligned slab size, and
    /// calls `get_aligned` with a `Layout` describing that size. `get_aligned` returns an `Option`
    ///  - `None` if the requested size is not supported, and `Some` if the size is supported. If
    /// the size is not supported, `build_backing` will fall back to using large slabs, and will
    /// call `get_large` to get an allocator. It will only call `get_large` with a `Layout` that is
    /// required to be supported (at least a page in size and page-aligned), so `get_large` returns
    /// an allocator directly rather than an `Option`.
    pub fn build_backing<B, A, L>(self, get_aligned: A, get_large: L) -> UntypedSlabAlloc<I, B>
    where
        B: BackingAlloc,
        A: Fn(Layout) -> Option<B::Aligned>,
        L: Fn(Layout) -> B::Large,
    {
        let layout = util::misc::satisfy_min_align(self.layout.clone(), I::min_align());
        let aligned_backing_size = aligned::backing_size_for::<I>(&layout);
        let aligned_slab_layout =
            Layout::from_size_align(aligned_backing_size, aligned_backing_size).unwrap();
        UntypedSlabAlloc {
            alloc: if let Some(alloc) = get_aligned(aligned_slab_layout) {
                let data = aligned::System::new(layout, alloc).unwrap();
                PrivateUntypedSlabAlloc::Aligned(SizedSlabAlloc::new(self.init, self.layout, data))
            } else {
                let backing_size = large::backing_size_for::<I>(&layout);
                let slab_layout = Layout::from_size_align(backing_size, *PAGE_SIZE).unwrap();
                let data = large::System::new(layout, get_large(slab_layout)).unwrap();
                PrivateUntypedSlabAlloc::Large(SizedSlabAlloc::new(self.init, self.layout, data))
            },
        }
    }
}

impl<F: Fn(NonNull<u8>)> UntypedSlabAllocBuilder<UnsafeFnInitSystem<u8, F>> {
    pub fn func(layout: Layout, f: F) -> UntypedSlabAllocBuilder<UnsafeFnInitSystem<u8, F>> {
        UntypedSlabAllocBuilder {
            init: UnsafeFnInitSystem::new(UnsafeFnInitializer::new(f)),
            layout: layout,
        }
    }
}

impl UntypedSlabAllocBuilder<NopInitSystem> {
    pub fn new(layout: Layout) -> UntypedSlabAllocBuilder<NopInitSystem> {
        UntypedSlabAllocBuilder {
            init: NopInitSystem,
            layout: layout,
        }
    }
}

unsafe impl<T, I: InitSystem, B: BackingAlloc> ObjectAlloc<T> for SlabAlloc<T, I, B> {
    unsafe fn alloc(&mut self) -> Option<NonNull<T>> {
        match self.alloc {
            PrivateSlabAlloc::Aligned(ref mut alloc) => alloc.alloc(),
            PrivateSlabAlloc::Large(ref mut alloc) => alloc.alloc(),
        }.map(NonNull::cast)
    }

    unsafe fn dealloc(&mut self, x: NonNull<T>) {
        match self.alloc {
            PrivateSlabAlloc::Aligned(ref mut alloc) => alloc.dealloc(x.cast()),
            PrivateSlabAlloc::Large(ref mut alloc) => alloc.dealloc(x.cast()),
        }
    }
}

unsafe impl<T, I: InitSystem, B: BackingAlloc> UntypedObjectAlloc for SlabAlloc<T, I, B> {
    fn layout(&self) -> Layout {
        match self.alloc {
            PrivateSlabAlloc::Aligned(ref alloc) => alloc.layout.clone(),
            PrivateSlabAlloc::Large(ref alloc) => alloc.layout.clone(),
        }
    }

    unsafe fn alloc(&mut self) -> Option<NonNull<u8>> {
        match self.alloc {
            PrivateSlabAlloc::Aligned(ref mut alloc) => alloc.alloc(),
            PrivateSlabAlloc::Large(ref mut alloc) => alloc.alloc(),
        }
    }

    unsafe fn dealloc(&mut self, x: NonNull<u8>) {
        match self.alloc {
            PrivateSlabAlloc::Aligned(ref mut alloc) => alloc.dealloc(x),
            PrivateSlabAlloc::Large(ref mut alloc) => alloc.dealloc(x),
        }
    }
}

unsafe impl<I: InitSystem, B: BackingAlloc> UntypedObjectAlloc for UntypedSlabAlloc<I, B> {
    fn layout(&self) -> Layout {
        match self.alloc {
            PrivateUntypedSlabAlloc::Aligned(ref alloc) => alloc.layout.clone(),
            PrivateUntypedSlabAlloc::Large(ref alloc) => alloc.layout.clone(),
        }
    }

    unsafe fn alloc(&mut self) -> Option<NonNull<u8>> {
        match self.alloc {
            PrivateUntypedSlabAlloc::Aligned(ref mut alloc) => alloc.alloc(),
            PrivateUntypedSlabAlloc::Large(ref mut alloc) => alloc.alloc(),
        }
    }

    unsafe fn dealloc(&mut self, x: NonNull<u8>) {
        match self.alloc {
            PrivateUntypedSlabAlloc::Aligned(ref mut alloc) => alloc.dealloc(x),
            PrivateUntypedSlabAlloc::Large(ref mut alloc) => alloc.dealloc(x),
        }
    }
}

struct SizedSlabAlloc<I: InitSystem, S: SlabSystem<I>> {
    freelist: LinkedList<S::Slab>, // partial slabs first, followed by full slabs
    total_slabs: usize,
    num_full: usize, // number of full slabs
    refcnt: usize,
    // minimum number of slabs full at every moment during this working period
    full_slab_working_set: WorkingSet<usize>,

    slab_system: S,
    init_system: I,

    layout: Layout,
}

impl<I: InitSystem, S: SlabSystem<I>> SizedSlabAlloc<I, S> {
    fn new(init: I, layout: Layout, slabs: S) -> SizedSlabAlloc<I, S> {
        SizedSlabAlloc {
            freelist: LinkedList::new(),
            total_slabs: 0,
            num_full: 0,
            refcnt: 0,
            full_slab_working_set: WorkingSet::new(0),
            slab_system: slabs,
            init_system: init,
            layout: layout,
        }
    }

    fn alloc(&mut self) -> Option<NonNull<u8>> {
        if self.freelist.size() == 0 {
            let ok = self.alloc_slab();
            if !ok {
                return None;
            }
        }

        let slab = self.freelist.peek_front();
        if self.slab_system.is_full(slab) {
            self.num_full -= 1;
            self.full_slab_working_set.update_min(self.num_full);
        }

        let (obj, init_status) = self.slab_system.alloc(slab);
        if self.slab_system.is_empty(slab) {
            self.freelist.remove_front();
        }
        self.refcnt += 1;
        debug_assert_eq!(obj.as_ptr() as usize % self.layout.align(), 0);
        self.init_system.init(obj, init_status);
        Some(obj)
    }

    /// Allocate a new slab.
    ///
    /// Allocates a new slab and inserts it onto the back of the freelist. Returns `true` upon
    /// success and `false` upon failure.
    fn alloc_slab(&mut self) -> bool {
        if let Some(new) = self.slab_system.alloc_slab() {
            // technically it doesn't matter whether it's back or front since this is only called when
            // the list is currently empty
            self.freelist.insert_back(new);
            self.total_slabs += 1;
            self.num_full += 1;
            true
        } else {
            false
        }
    }

    fn dealloc(&mut self, ptr: NonNull<u8>) {
        debug_assert_eq!(ptr.as_ptr() as usize % self.layout.align(), 0);
        let (slab, was_empty) = self.slab_system.dealloc(ptr, I::status_initialized());
        let is_full = self.slab_system.is_full(slab);

        match (was_empty, is_full) {
            // !was_empty implies it's already in the freelist; is_full implies it should be
            // moved to the back of the freelist
            (false, true) => {
                self.freelist.move_to_back(slab);
                self.num_full += 1;
            }
            // was_empty implies it's not already in the freelist; is_full implies it should be
            // added to the back of the freelist (note: only possible if slabs have size 1 -
            // they go from empty to full in a single free)
            (true, true) => {
                self.freelist.insert_back(slab);
                self.num_full += 1;
            }
            // was_empty implies it's not already in the freelist; !is_full implies it should
            // be added to the front since it's now partially-full
            (true, false) => self.freelist.insert_front(slab),
            // !was_empty implies it's already in the front section of the freelist; !is_full
            // implies it should stay there
            (false, false) => {}
        }

        if is_full {
            // TODO: document the logic behind only doing this when a slab becomes full
            self.garbage_collect_slabs();
        }

        self.refcnt -= 1;
    }

    fn garbage_collect_slabs(&mut self) {
        if let Some(min_full) = self.full_slab_working_set.refresh(WORKING_PERIOD_SECONDS) {
            for _ in 0..min_full {
                let slab = self.freelist.remove_back();
                self.slab_system.dealloc_slab(slab);
                self.total_slabs -= 1;
                self.num_full -= 1;
            }
            self.full_slab_working_set.set(self.num_full);
        }
    }
}

impl<I: InitSystem, S: SlabSystem<I>> Drop for SizedSlabAlloc<I, S> {
    fn drop(&mut self) {
        if self.refcnt != 0 {
            if std::thread::panicking() {
                // TODO: We shouldn't panic here because then we'd panic while panicking, and just
                // abort without printing a useful message. We should figure out an alternative
                // that allows us to properly print a diagnostic.
            } else {
                panic!("non-zero refcount when dropping slab allocator");
            }
        }

        while self.freelist.size() > 0 {
            let slab = self.freelist.remove_front();
            self.slab_system.dealloc_slab(slab);
        }
    }
}

trait SlabSystem<I: InitSystem> {
    type Slab: Linkable;

    /// Allocate a new `Slab`.
    ///
    /// The returned `Slab` has its next and previous pointers initialized to `None`.
    fn alloc_slab(&mut self) -> Option<NonNull<Self::Slab>>;
    fn dealloc_slab(&mut self, slab: NonNull<Self::Slab>);

    /// `is_full` returns true if all objects are available for allocation.
    fn is_full(&self, slab: NonNull<Self::Slab>) -> bool;
    /// `is_empty` returns true if no objects are available for allocation.
    fn is_empty(&self, slab: NonNull<Self::Slab>) -> bool;
    /// `alloc` allocates a new object from the given `Slab`.
    fn alloc(&self, slab: NonNull<Self::Slab>) -> (NonNull<u8>, I::Status);
    /// `dealloc` deallocates the given object. It is `dealloc`'s responsibility to find the
    /// object's parent `Slab` and return it. It also returns whether the `Slab` was empty prior to
    /// deallocation.
    fn dealloc(&self, obj: NonNull<u8>, init_status: I::Status) -> (NonNull<Self::Slab>, bool);
}
