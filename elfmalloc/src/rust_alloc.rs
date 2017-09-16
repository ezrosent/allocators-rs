// Copyright 2017 the authors. See the 'Copyright and license' section of the
// README.md file at the top-level directory of this repository.
//
// Licensed under the Apache License, Version 2.0 (the LICENSE-APACHE file) or
// the MIT license (the LICENSE-MIT file) at your option. This file may not be
// copied, modified, or distributed except according to those terms.

//! A version of elfmalloc tailored to the `Alloc` trait.
//!
//! This module is an attempt at creating a variant of elfmalloc that follows Rust's own allocation
//! idioms. Unlike the traditional `malloc`/`free` interface, Rust's `Alloc` trait passes size and
//! alignment at the call site for both allocations and deallocations. This allows us to simplify
//! the design in several ways.
//!
//! # Differences in Design
//!
//! The design is still quite similar to that of the standard `elfmalloc` implementation. In fact,
//! the implementation of this rust-specific variant is cobbled together out of different reusable
//! components from the initial allocator. We briefly mention the newer elements of the design
//! below.
//!
//! ## Small Objects
//!
//! Small objects are allocated much the same as they are in `elfmalloc`: we use the slab allocation
//! system present in the `slag` module. The main difference is that the slag page size can be
//! relatively small (several KiB rather than a few MiB), but also allowed to filled usable space.
//! This is because the slab subsystem *only* handles smaller allocations, rather than handling
//! both small and medium sized objects. This has the advantage that it doesn't rely on holding
//! large chunks of uncommitted memory around. That's a good thing for 2 reasons:
//!
//! * Windows requires memory to be committed explicitly. For a `malloc`, this will require
//!   explicit bookkeeping for which slabs are fully committed and which are only using a small
//!   portion of the available space. If slabs are always fully committed, this is no longer a
//!   problem.
//!
//! * For 32-bit support, we cannot be so profligate with virutal memory. Committing to only the
//!   memory we (physically) require is a good step towards getting 32-bit machines fully supported.
//!
//! ## Medium Objects
//!
//! So if each slab is only, say, 32K, what do we do for larger objects? In the standard malloc
//! interface, we simply fell back on `mmap` directly. Now that the slab subsystem has a smaller
//! maximum size (roughly a fourth of he slab size), we want a more scalable solution for objects
//! that are still relatively small. These medium-sized objects are instead allocated from
//! `BagPipe`s directly. We use a tiered system of `PageAlloc`-like structures to provide
//! size-class specific allocation for a limited number of cached objects, falling back on a global
//! `BagPipe` that is shared among all large size classes.
//!
//! This means that the large size classes all share the same underlying page size, meaning that it
//! is possible to waste some virtual memory space. The good news is that the degree to which this
//! space is wasted is configurable. Still, more work may be required for full support of the
//! 32-bit setting.
//!
//! ## Large Objects
//!
//! Large objects simply call `mmap` and `munmap` directly. Indeed, no additional metadata needs to
//! be stored for these operations because size information is provided by the caller of `dealloc`.
//!
//! ## Backing Allocation
//!
//! The `malloc`-style `elfmalloc` implementation is essentially a composition of two separate
//! allocators. On the one hand, there is the "ensemble of slabs" allocator. On the other, there
//! are the "large" allocations which have a different method for storing metadata. We crucially
//! must have a means of determining which of these allocators corresponds to a given pointer
//! passed to `free`. Our solution is to use the `Creek` data-structure: a means of ensuring all
//! (and only) slab-allocated memory belongs to the same (large) contiguous region of memory. The
//! `Creek`, however, is cumbersome to use: overcommitted memory has different support across
//! different operating systems, and it essentially locks us into using 64-bit architectures.
//!
//! In this module, memory is backed by a much thinner wrapper around `mmap`. We can get away with
//! this because we can use the size passed in at the call site to determine the allocator to which
//! a given object belongs. This is also the trick that allows us to handle medium objects
//! specially: in the other system, they would need their own `Creek`.
extern crate num_cpus;

use super::alloc::allocator::{Alloc, AllocErr, Layout};
use super::general::{Multiples, PowersOfTwo, ObjectAlloc, MULTIPLE, AllocMap};
use super::slag::{PageAlloc, Metadata, RevocablePipe, compute_metadata, SlagPipe};
use super::utils::{mmap, Lazy, LazyInitializable};
use super::sources::MemorySource;
use super::bagpipe::bag::WeakBag;
use super::sources::MmapSource;

use std::cmp;
use std::mem;
use std::ptr;

/// A shared concurrent data-structure for caching large objects.
///
/// A single `PageSource` is used as a backing store for several `PageFrontend`s, each with a
/// separate object size less than or equal to the page size of `M`. This structure allows for
/// unused pages freed from one size class to be used to service allocations in another size class.
///
/// The `PageSource` can be configured to decommit a portion of memory reclaimed from object
/// classes whose size exceeds a certain threshold.
#[derive(Clone)]
struct PageSource<M: MemorySource> {
    cutoff_bytes: usize,
    target_size: usize,
    pages: SlagPipe<u8>,
    source: M,
}

impl<M: MemorySource> PageSource<M> {
    fn new(
        cutoff_bytes: usize,
        target_size: usize,
        pipe_size: usize,
        page_size: usize,
    ) -> PageSource<M> {
        PageSource {
            cutoff_bytes: cutoff_bytes,
            target_size: target_size,
            pages: SlagPipe::new_size(pipe_size),
            source: M::new(page_size),
        }
    }

    unsafe fn free(&mut self, p: *mut u8, old_size: usize) {
        if self.pages.size_guess() >= self.target_size as isize {
            mmap::unmap(p, self.source.page_size());
            return;
        }
        if old_size >= self.cutoff_bytes {
            mmap::uncommit(
                p.offset(self.cutoff_bytes as isize),
                self.source.page_size() - self.cutoff_bytes,
            );
        }
        self.pages.push_mut(p);
    }

    unsafe fn alloc(&mut self) -> Option<*mut u8> {
        self.pages.pop_mut().or_else(|| {
            const NPAGES: usize = 4;
            self.source.carve(NPAGES).and_then(|pages| {
                for i in 1..NPAGES {
                    let offset = (i * self.source.page_size()) as isize;
                    self.pages.push_mut(pages.offset(offset));
                }
                Some(pages)
            })
        })
    }
}


/// An allocator used for allocating large objects.
///
/// A `PageFrontend` is a thin wrapper around a `BagPipe`, where additional memory is acquired from
/// a `PageSource`.
#[derive(Clone)]
struct PageFrontend<M: MemorySource> {
    parent: PageSource<M>,
    pages: SlagPipe<u8>,
    local_size: usize,
    max_overhead: usize,
}

impl<M: MemorySource> PageFrontend<M> {
    fn new(
        size: usize,
        max_overhead: usize,
        pipe_size: usize,
        parent: PageSource<M>,
    ) -> PageFrontend<M> {
        debug_assert!(size <= parent.source.page_size());
        PageFrontend {
            parent: parent,
            pages: SlagPipe::new_size(pipe_size),
            local_size: size,
            max_overhead: max_overhead,
        }
    }

    unsafe fn alloc(&mut self) -> Option<*mut u8> {
        self.pages.pop_mut().or_else(|| self.parent.alloc())
    }

    unsafe fn free(&mut self, item: *mut u8) {
        if self.pages.size_guess() >= self.max_overhead as isize {
            self.parent.free(item, self.local_size);
            return;
        }
        self.pages.try_push_mut(item).unwrap_or_else(|_| {
            self.parent.free(item, self.local_size);
        })
    }
}

impl<M: MemorySource + Clone> LazyInitializable for PageFrontend<M> {
    type Params = (usize, usize, usize, PageSource<M>);
    fn init(&(size, max_overhead, pipe_size, ref parent): &Self::Params) -> Self {
        PageFrontend::new(size, max_overhead, pipe_size, parent.clone())
    }
}

/// An `Alloc` impl that takes advantage of size information at the call-site to more efficiently
/// handle larger allocations.
#[derive(Clone)]
pub struct ElfMalloc<M: MemorySource> {
    small: Multiples<ObjectAlloc<PageAlloc<M>>>,
    large: PowersOfTwo<Lazy<PageFrontend<M>>>,
}

/// Following the structure of the `general` module, we keep the underlying `ElfMalloc` struct with
/// a trivial drop method, as drop needs to be specialized when the allocator is and isn't used in
/// thread local storage.
///
/// `OwnedElfMalloc` is the standard version, where the destructor reclaims any cached memory.
#[derive(Clone)]
pub struct OwnedElfMalloc<M: MemorySource>(pub ElfMalloc<M>);


impl<M: MemorySource> ::std::ops::Deref for OwnedElfMalloc<M> {
    type Target = ElfMalloc<M>;
    fn deref(&self) -> &ElfMalloc<M> {
        &self.0
    }
}

impl<M: MemorySource> ::std::ops::DerefMut for OwnedElfMalloc<M> {
    fn deref_mut(&mut self) -> &mut ElfMalloc<M> {
        &mut self.0
    }
}

unsafe impl<M: MemorySource + Send> Send for ElfMalloc<M> {}
unsafe impl<M: MemorySource + Send> Send for OwnedElfMalloc<M> {}

impl<M: MemorySource> ElfMalloc<M> {
    unsafe fn destroy(&mut self) {
        self.small.foreach(|x| ptr::drop_in_place(x));
        self.large.foreach(|x| ptr::drop_in_place(x));
        self.small.classes.destroy();
        self.large.classes.destroy();
    }
}

impl<M: MemorySource> Drop for OwnedElfMalloc<M> {
    fn drop(&mut self) {
        unsafe {
            self.0.destroy();
        }
    }
}


macro_rules! case_analyze {
    ($self:expr, $layout:expr, small $small:expr; medium $medium:expr; large $large:expr;) => {
        {
            if $layout.size() <= $self.small.max_key() {
                $small
            } else if $layout.size() <= $self.large.max_key() {
                $medium
            } else {
                $large
            }
        }
    }
}

unsafe impl<M: MemorySource> Alloc for ElfMalloc<M> {
    #[inline(always)]
    unsafe fn alloc(&mut self, l: Layout) -> Result<*mut u8, AllocErr> {
        trace!("alloc({:?})", l);
        case_analyze!(
            self,
            l,
            small Ok(
                self.small
                    .get_mut(if l.align() > mem::size_of::<usize>() {
                        l.size().next_power_of_two()
                    } else {
                        l.size()
                    })
                    .alloc(),
            );
            medium match self.large.get_mut(l.size()).alloc() {
                Some(p) => Ok(p),
                None => Err(AllocErr::Exhausted { request: l }),
            };
            large match mmap::fallible_map(l.size()) {
                Some(p) => Ok(p),
                None => Err(AllocErr::Exhausted { request: l }),
            };
        )
    }

    #[inline(always)]
    unsafe fn dealloc(&mut self, item: *mut u8, l: Layout) {
        trace!("dealloc({:?}, {:?})", item, l);
        case_analyze!(
            self,
            l,
            small self.small.get_mut(if l.align() > mem::size_of::<usize>() {
                        l.size().next_power_of_two()
                    } else {
                        // round up to nearest MULTIPLE
                        (l.size() + (MULTIPLE - 1)) & !(MULTIPLE - 1)
                    }).free(item);
            medium self.large.get_mut(l.size()).free(item);
            large mmap::unmap(item, l.size());)
    }

    #[inline(always)]
    fn usable_size(&self, l: &Layout) -> (usize, usize) {
        trace!("usable_size({:?})", l.clone());
        (
            l.size(),
            case_analyze!(
            self,
            l,
            small if l.align() > mem::size_of::<usize>() {
                l.size().next_power_of_two()
            } else {
                l.size()
            };
            medium l.size().next_power_of_two();
            large l.size();),
        )
    }
}

unsafe impl<M: MemorySource> Alloc for OwnedElfMalloc<M> {
    unsafe fn alloc(&mut self, l: Layout) -> Result<*mut u8, AllocErr> {
        self.0.alloc(l)
    }

    unsafe fn dealloc(&mut self, p: *mut u8, l: Layout) {
        self.0.dealloc(p, l)
    }

    fn usable_size(&self, l: &Layout) -> (usize, usize) {
        self.0.usable_size(l)
    }
}

pub struct ElfMallocBuilder {
    page_size: usize,
    target_pa_size: usize,
    reuse_threshold: f64,
    max_object_size: usize,
    small_pipe_size: usize,
    large_pipe_size: usize,

    large_obj_cutoff: usize,
    large_obj_target_size: usize,
    target_pipe_overhead: usize,
}

impl Default for ElfMallocBuilder {
    fn default() -> ElfMallocBuilder {
        ElfMallocBuilder {
            page_size: 32 << 10,
            // very large -- this effectively turns the feature off
            target_pa_size: 1 << 30,
            reuse_threshold: 0.6,
            max_object_size: 8 << 20,
            small_pipe_size: cmp::max(1, num_cpus::get() / 4),
            large_pipe_size: cmp::max(1, num_cpus::get() / 2),
            large_obj_cutoff: 1 << 20,
            large_obj_target_size: 1 << 12,
            target_pipe_overhead: 16 << 20,
        }
    }
}

impl ElfMallocBuilder {
    pub fn page_size(&mut self, page_size: usize) -> &mut ElfMallocBuilder {
        self.page_size = cmp::max(mmap::page_size(), page_size);
        self
    }
    pub fn target_pa_size(&mut self, target_pa_size: usize) -> &mut ElfMallocBuilder {
        self.target_pa_size = target_pa_size;
        self
    }
    pub fn reuse_threshold(&mut self, reuse_threshold: f64) -> &mut ElfMallocBuilder {
        self.reuse_threshold = reuse_threshold;
        self
    }
    pub fn max_object_size(&mut self, max_object_size: usize) -> &mut ElfMallocBuilder {
        self.max_object_size = max_object_size;
        self
    }
    pub fn small_pipe_size(&mut self, small_pipe_size: usize) -> &mut ElfMallocBuilder {
        self.small_pipe_size = small_pipe_size;
        self
    }
    pub fn large_pipe_size(&mut self, large_pipe_size: usize) -> &mut ElfMallocBuilder {
        self.large_pipe_size = large_pipe_size;
        self
    }

    pub fn build<M: MemorySource>(&self) -> ElfMalloc<M> {
        let pa = PageAlloc::<M>::new(self.page_size, self.target_pa_size, self.large_pipe_size);
        let n_small_classes = (self.page_size / 4) / MULTIPLE;
        assert!(n_small_classes > 0);
        let mut meta_pointers = mmap::map(mem::size_of::<Metadata>() * n_small_classes) as
            *mut Metadata;
        let small_classes = Multiples::init(MULTIPLE, n_small_classes, |size: usize| {
            let meta = meta_pointers;
            unsafe {
                meta_pointers = meta_pointers.offset(1);
                ptr::write(
                    meta,
                    compute_metadata(
                        size,
                        self.page_size,
                        0,
                        self.reuse_threshold,
                        self.page_size,
                    ),
                );
            }
            let params = (
                meta,
                usize::max_value(), /* no eager decommit */
                pa.clone(),
                RevocablePipe::new_size(self.small_pipe_size),
            );
            ObjectAlloc::new(params)
        });
        let next_size_class = (small_classes.max_key() + 1).next_power_of_two();
        let max_size = self.max_object_size.next_power_of_two();
        let n_classes = max_size.trailing_zeros() - next_size_class.trailing_zeros();
        let p_source = PageSource::<M>::new(
            self.large_obj_cutoff,
            self.large_obj_target_size,
            self.large_pipe_size,
            max_size,
        );
        let large_classes = PowersOfTwo::init(next_size_class, n_classes as usize, |size: usize| {
            let target_size: usize = cmp::max(1, self.target_pipe_overhead / size);
            Lazy::<PageFrontend<M>>::new(
                (size, target_size, self.small_pipe_size, p_source.clone()),
            )
        });
        debug_assert!(small_classes.max_key().is_power_of_two());
        ElfMalloc {
            small: small_classes,
            large: large_classes,
        }
    }

    pub fn build_owned<M: MemorySource>(&self) -> OwnedElfMalloc<M> {
        OwnedElfMalloc(self.build())
    }
}

pub use self::global::{DynamicAlloc, SharedAlloc, new_owned_handle};

mod global {
    //! This module provides an interface to global instances of allocators in the parent module.
    //! Two interfaces are provides. All allocations from either of these allocators share the same
    //! global data-structures.
    //!
    //! # `DynamicAlloc`
    //!
    //! This allocator is a complete instance of the `ElfMalloc` frontend. When its destructor is
    //! called, any cached memory is returned to the global pool of memory. A `DynamicAlloc` can be
    //! embedded in a long-lived data-structure and grant its allocations some degree of isolation
    //! from other components of the current thread. Embedding the allocator in this manner also
    //! ensure that allocations and frees are cached correctly even when the allocator is moved to
    //! a different thread.
    //!
    //! # `SharedAlloc`
    //!
    //! This allocator stores a single frontend per thread in thread-local storage (TLS). It
    //! therefore allows different data-structures to share a single frontend. In experiments with
    //! a custom `Vec` implementation, this leads to serious performance wins.
    use super::*;

    use std::intrinsics::unlikely;
    use std::sync::mpsc::{channel, Sender};
    use std::sync::Mutex;
    use std::thread;
    use std::cell::UnsafeCell;

    type Source = MmapSource;
    type InnerAlloc = ElfMalloc<Source>;

    pub type DynamicAlloc = OwnedElfMalloc<Source>;

    /// A global handle that can hand out thread-local handles to the allocator.
    struct ElfCloner(InnerAlloc);

    impl ElfCloner {
        fn new_handle(&self) -> DynamicAlloc {
            OwnedElfMalloc(self.0.clone())
        }
    }
    unsafe impl Sync for ElfCloner {}

    /// Construct a new `DynamicAlloc`.
    pub fn new_owned_handle() -> DynamicAlloc {
        GLOBAL_HANDLE.new_handle()
    }

    lazy_static! {
        static ref GLOBAL_HANDLE: ElfCloner = ElfCloner(ElfMallocBuilder::default()
                                                        .page_size(16 << 10)
                                                        .build());

        /// We still have a crossbeam dependency, which means that we may have to reclaim a
        /// thread's cached memory after it is destroyed. See the comments in `general::global` for
        /// more context on why this is necessary.
        static ref BACKUP_CLEAN: Mutex<Sender<DynamicAlloc>> = {
            let (sender, receiver) = channel();
            thread::spawn(move || loop {
                if let Ok(msg) = receiver.recv() {
                    mem::drop(msg);
                }
            });
            Mutex::new(sender)
        };
    }

    #[thread_local]
    static mut RUST_PTR: *mut InnerAlloc = ptr::null_mut();

    /// A newtype wrapper around `ElfMalloc<Source>` that sends its contents to the background
    /// thread in its destructor.
    struct ElfMallocTLS(InnerAlloc);

    impl Drop for ElfMallocTLS {
        fn drop(&mut self) {
            unsafe {
                let _ = BACKUP_CLEAN.lock().unwrap().send(OwnedElfMalloc(
                    ptr::read(&mut self.0),
                ));
            }
        }
    }

    thread_local! {
        static LOCAL_RUSTMALLOC: UnsafeCell<ElfMallocTLS> =
            UnsafeCell::new(ElfMallocTLS(GLOBAL_HANDLE.0.clone()));
    }

    macro_rules! with_instance {

        ($ptr:ident, $exp:expr) => {
            with_instance!($ptr, $exp, panic!("TLS in invalid state"))
        };

        ($ptr:ident, $exp:expr, $else:expr) => {
            if unlikely(RUST_PTR.is_null()) {
                LOCAL_RUSTMALLOC.try_with(|uc| {
                    let $ptr: &mut InnerAlloc = &mut (*uc.get()).0;
                    let res = $exp;
                    RUST_PTR = $ptr;
                    res
                }).unwrap_or_else(|_| {
                    $else
                })
            } else {
                let $ptr = &mut *RUST_PTR;
                $exp
            }
        };

    }

    /// A ZST for routing allocations through the thread-local handle.
    #[derive(Clone)]
    pub struct SharedAlloc;

    unsafe impl Alloc for SharedAlloc {
        unsafe fn alloc(&mut self, l: Layout) -> Result<*mut u8, AllocErr> {
            with_instance!(r_ptr, r_ptr.alloc(l))
        }
        unsafe fn dealloc(&mut self, p: *mut u8, l: Layout) {
            with_instance!(r_ptr, r_ptr.dealloc(p, l))
        }
        fn usable_size(&self, l: &Layout) -> (usize, usize) {
            unsafe { with_instance!(r_ptr, r_ptr.usable_size(l)) }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::mem;
    use std::ptr;
    use std::thread;

    #[test]
    fn basic_alloc_functionality() {
        let word_size = mem::size_of::<usize>();
        let mut alloc = ElfMallocBuilder::default().build_owned::<MmapSource>();
        let layouts: Vec<_> = (word_size..(8 << 10))
            .map(|size| {
                Layout::from_size_align(size * 128, word_size).unwrap()
            })
            .collect();
        unsafe {
            let mut ptrs = Vec::new();
            for l in &layouts {
                let p = alloc.alloc(l.clone()).expect("alloc should not fail") as *mut usize;
                ptr::write_volatile(p, !0);
                ptrs.push(p);
            }
            for (ptr, l) in ptrs.into_iter().zip(layouts.into_iter()) {
                alloc.dealloc(ptr as *mut u8, l);
            }
        }
    }

    fn multi_threaded_alloc_test(layouts: Vec<Layout>) {
        const N_THREADS: usize = 64;
        let alloc = ElfMallocBuilder::default().build_owned::<MmapSource>();

        let mut threads = Vec::new();
        for _ in 0..N_THREADS {
            let mut my_alloc = alloc.clone();
            let my_layouts = layouts.clone();
            threads.push(thread::spawn(move || unsafe {
                for _ in 0..2 {
                    let mut ptrs = Vec::new();
                    for l in &my_layouts {
                        let p = my_alloc.alloc(l.clone()).expect("alloc should not fail") as
                            *mut usize;
                        ptr::write_volatile(p, !0);
                        ptrs.push(p);
                    }
                    for (ptr, l) in ptrs.into_iter().zip(my_layouts.iter()) {
                        my_alloc.dealloc(ptr as *mut u8, (*l).clone());
                    }
                }
            }))
        }
        for t in threads.into_iter() {
            t.join().expect("threads should not fail")
        }
    }

    #[test]
    fn many_threads_many_sizes() {
        let word_size = mem::size_of::<usize>();
        multi_threaded_alloc_test(
            (word_size..(2 << 10))
                .map(|size| {
                    Layout::from_size_align(size * 1024, word_size).unwrap()
                })
                .collect(),
        );
    }

    #[test]
    fn large_ws_small_size() {
        multi_threaded_alloc_test(
            (1..(8 << 10))
                .map(|_| Layout::from_size_align(64, 64).unwrap())
                .collect(),
        );
    }

    #[test]
    fn large_ws_large_size() {
        multi_threaded_alloc_test(
            (1..(1 << 8))
                .map(|_| Layout::from_size_align(512 << 10, 64).unwrap())
                .collect(),
        );
    }
}
