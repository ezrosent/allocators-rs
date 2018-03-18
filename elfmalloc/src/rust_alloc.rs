// Copyright 2017-2018 the authors. See the 'Copyright and license' section of the
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

use std::{cmp, mem};
use std::ptr::NonNull;
use std::sync::Arc;

use alloc::allocator::{Alloc, AllocErr, Layout};
use alloc_fmt::AllocUnwrap;
use bagpipe::BagPipe;
use bagpipe::bag::WeakBag;
use bagpipe::queue::FAAQueueLowLevel;
use mmap_alloc::{MapAlloc, MapAllocBuilder};
use object_alloc::UntypedObjectAlloc;

use alloc_map::{AllocMap, Multiples, PowersOfTwo};
use alloc_type::AllocType;
use backing::{GCAlloc, PageCache};
#[allow(unused_imports)]
use frontends::{Depot, Frontend};
use general::{ObjectAlloc, Sixteen};
use slag::{compute_metadata, Metadata, PageCleanup, SlagConfig};
use util::{mmap, AllocWith, ConfigBuild, Const, Lazy, LazyGuard, MmapVec, PreDrop, TryClone};

type SlagPipe = BagPipe<FAAQueueLowLevel<*mut u8>, PageCleanup<u8>>;

// TODO: Could we merge PageSource and PageCache? They're pretty similar.

/// A shared concurrent data-structure for caching large objects.
///
/// A single `PageSource` is used as a backing store for several `PageFrontend`s, each with a
/// separate object size less than or equal to the page size of `M`. This structure allows for
/// unused pages freed from one size class to be used to service allocations in another size class.
///
/// The `PageSource` can be configured to decommit a portion of memory reclaimed from object
/// classes whose size exceeds a certain threshold.
#[derive(Clone)]
struct PageSource {
    cutoff_bytes: usize,
    target_cached_pages: usize,
    pages: SlagPipe,
    alloc: MapAlloc,
}

impl PageSource {
    fn new(
        cutoff_bytes: usize,
        target_cached_pages: usize,
        num_pipes: usize,
        alloc: MapAlloc,
    ) -> PageSource {
        let page_size = alloc.layout().size();
        alloc_assert!(page_size > cutoff_bytes);
        PageSource {
            cutoff_bytes,
            target_cached_pages,
            pages: SlagPipe::new_size_cleanup(num_pipes, PageCleanup::new(page_size)),
            alloc,
        }
    }

    unsafe fn dealloc_old_size(&mut self, guard: &LazyGuard, p: NonNull<u8>, old_size: usize) {
        if self.pages.size_guess() >= self.target_cached_pages as isize {
            <MapAlloc as UntypedObjectAlloc>::dealloc(&mut self.alloc, p);
            return;
        }
        if old_size >= self.cutoff_bytes {
            let ptr = NonNull::new_unchecked(p.as_ptr().offset(self.cutoff_bytes as isize));
            let layout = Layout::from_size_align(self.alloc.layout().size() - self.cutoff_bytes, 1).alloc_unwrap();
            self.alloc.uncommit(ptr.as_ptr(), layout);
        }
        self.pages.push_mut(guard.guard(), p.as_ptr());
    }
}

unsafe impl GCAlloc for PageSource {
    fn layout(&self) -> Layout {
        self.alloc.layout()
    }

    unsafe fn alloc(&mut self, guard: &LazyGuard) -> Option<NonNull<u8>> {
        self.pages.pop_mut(guard.guard())
            .map(|ptr| NonNull::new_unchecked(ptr))
            .or_else(|| <MapAlloc as UntypedObjectAlloc>::alloc(&mut self.alloc))
    }

    // NOTE: All of the code in this module calls dealloc_old_size. This method
    // is only called by pre_drop implementations, which are, in practice, only
    // called when an ElfMalloc handle is being dropped. We conservatively
    // uncommit the entire object since the caller can't pass an old_size
    // parameter.
    unsafe fn dealloc(&mut self, guard: &LazyGuard, p: NonNull<u8>) {
        let old_size = self.alloc.layout().size();
        self.dealloc_old_size(guard, p, old_size);
    }
}

/// An allocator used for allocating large objects.
///
/// A `PageFrontend` is a thin wrapper around a `BagPipe`, where additional memory is acquired from
/// a `PageSource`.
#[derive(Clone)]
struct PageFrontend {
    pages: SlagPipe,
    local_size: usize,
    max_overhead: usize,
}

impl PageFrontend {
    fn new(
        size: usize,
        max_overhead: usize,
        pipe_size: usize,
    ) -> PageFrontend {
        alloc_debug_assert!(size.is_power_of_two());
        PageFrontend {
            pages: SlagPipe::new_size_cleanup(pipe_size, PageCleanup::new(size)),
            local_size: size,
            max_overhead: max_overhead,
        }
    }
}

unsafe impl AllocWith<PageSource> for PageFrontend {
    unsafe fn alloc_with(&mut self, guard: &LazyGuard, source: &mut PageSource) -> Option<NonNull<u8>> {
        self.pages.pop_mut(guard.guard())
            .map(|ptr| NonNull::new_unchecked(ptr))
            .or_else(|| source.alloc(guard))
    }

    unsafe fn dealloc_with(&mut self, guard: &LazyGuard, source: &mut PageSource, item: NonNull<u8>) {
        if self.pages.size_guess() >= self.max_overhead as isize {
            source.dealloc_old_size(guard, item, self.local_size);
            return;
        }
        self.pages.try_push_mut(guard.guard(), item.as_ptr()).unwrap_or_else(|_| {
            source.dealloc_old_size(guard, item, self.local_size);
        })
    }
}

#[derive(Clone)] // so PageFrontendConfig impls TryClone
struct PageFrontendConfig {
    size: usize,
    max_overhead: usize,
    pipe_size: usize,
}

impl<B> ConfigBuild<B> for PageFrontend {
    type Config = PageFrontendConfig;
    fn build(cfg: &Self::Config, _: &LazyGuard, _: &mut B) -> Option<Self> {
        Some(PageFrontend::new(cfg.size, cfg.max_overhead, cfg.pipe_size))
    }
}

/// An `Alloc` impl that takes advantage of size information at the call-site to more efficiently
/// handle larger allocations.
pub struct ElfMalloc {
    small: Multiples<ObjectAlloc<PageCache<MapAlloc>>, Sixteen>,
    medium: PowersOfTwo<Lazy<PageFrontend, PageSource>>,
    small_pages: PageCache<MapAlloc>,
    medium_pages: PageSource,
    metadata: Arc<MmapVec<Metadata>>,
}

impl Drop for ElfMalloc {
    fn drop(&mut self) {
        unsafe { self.small.pre_drop(&LazyGuard::new(), &mut self.small_pages) };
    }
}

macro_rules! case_analyze {
    ($self:expr, $layout:expr, small => $small:expr, medium => $medium:expr, large => $large:expr,) => {
        {
            if $layout.size() <= $self.small.max_key() {
                $small
            } else if $layout.size() <= $self.medium.max_key() {
                $medium
            } else {
                $large
            }
        }
    }
}

unsafe impl Alloc for ElfMalloc {
    #[inline(always)]
    unsafe fn alloc(&mut self, l: Layout) -> Result<*mut u8, AllocErr> {
        trace!("alloc({:?})", l);
        let guard = LazyGuard::new();
        case_analyze!(
            self,
            l,
            small => self.small.get_mut(if l.align() > mem::size_of::<usize>() {
                        l.size().next_power_of_two()
                    } else {
                        l.size()
                    })
                    .alloc_with(&guard, &mut self.small_pages)
                    .map(NonNull::as_ptr)
                    .ok_or(AllocErr::Exhausted { request: l }),
            medium => self.medium.get_mut(l.size()).alloc_with(&guard, &mut self.medium_pages)
                        .map(NonNull::as_ptr)
                        .ok_or(AllocErr::Exhausted { request: l }),
            large => match mmap::fallible_map(l.size()) {
                Some(p) => Ok(p),
                None => Err(AllocErr::Exhausted { request: l }),
            },
        )
    }

    #[inline(always)]
    unsafe fn dealloc(&mut self, item: *mut u8, l: Layout) {
        trace!("dealloc({:?}, {:?})", item, l);
        let guard = LazyGuard::new();
        case_analyze!(
            self,
            l,
            small => self.small.get_mut(if l.align() > mem::size_of::<usize>() {
                        l.size().next_power_of_two()
                    } else {
                        // round up to nearest MULTIPLE
                        (l.size() + (Sixteen::VAL - 1)) & !(Sixteen::VAL - 1)
                    }).dealloc_with(&guard, &mut self.small_pages, NonNull::new_unchecked(item)),
            medium => self.medium.get_mut(l.size())
                        .dealloc_with(
                            &guard,
                            &mut self.medium_pages,
                            NonNull::new_unchecked(item)
                         ),
            large => mmap::unmap(item, l.size()),
        )
    }

    #[inline(always)]
    fn usable_size(&self, l: &Layout) -> (usize, usize) {
        trace!("usable_size({:?})", l.clone());
        (
            l.size(),
            case_analyze!(
                self,
                l,
                small => if l.align() > mem::size_of::<usize>() {
                    l.size().next_power_of_two()
                } else {
                    l.size()
                },
                medium => l.size().next_power_of_two(),
                large => l.size(),
            ),
        )
    }
}

impl Clone for ElfMalloc {
    fn clone(&self) -> ElfMalloc {
        // TODO: Avoid leaking resources on failure (see issue #179)
        ElfMalloc {
            small: self.small.try_clone().alloc_expect("out of memory"),
            medium: self.medium.try_clone().alloc_expect("out of memory"),
            small_pages: self.small_pages.try_clone().alloc_expect("out of memory"),
            medium_pages: self.medium_pages.clone(),
            metadata: self.metadata.clone(),
        }
    }
}

impl Default for ElfMalloc {
    fn default() -> ElfMalloc {
        ElfMallocBuilder::default().build()
    }
}

pub struct ElfMallocBuilder {
    pub page_size: usize,
    pub target_pa_size: usize,
    pub reuse_threshold: f64,
    pub max_object_size: usize,
    pub small_pipe_size: usize,
    pub large_pipe_size: usize,

    pub large_obj_cutoff: usize,
    pub large_obj_target_size: usize,
    pub target_pipe_overhead: usize,
}

impl Default for ElfMallocBuilder {
    fn default() -> ElfMallocBuilder {
        ElfMallocBuilder {
            page_size: 32 << 10,
            // very large -- this effectively turns the feature off
            target_pa_size: 1 << 30,
            reuse_threshold: 0.6,
            max_object_size: 8 << 20,
            small_pipe_size: cmp::max(1, ::num_cpus::get() / 4),
            large_pipe_size: cmp::max(1, ::num_cpus::get() / 2),
            large_obj_cutoff: 1 << 20,
            large_obj_target_size: 1 << 12,
            target_pipe_overhead: 16 << 20,
        }
    }
}

impl ElfMallocBuilder {
    pub fn build(&self) -> ElfMalloc {
        let n_small_classes = (self.page_size / 4) / Sixteen::VAL;
        let min_medium_class = (n_small_classes * Sixteen::VAL + 1).next_power_of_two();
        let n_medium_classes = (self.max_object_size.next_power_of_two().trailing_zeros() - min_medium_class.trailing_zeros()) as usize;

        let mut metadata = MmapVec::new(n_small_classes + n_medium_classes).alloc_expect("out of memory");
        let (small, medium) = {
            let mut init_slag_config = |obj_size| {
                let meta = compute_metadata(obj_size, self.page_size, 0, self.reuse_threshold, self.page_size, AllocType::SmallSlag);
                unsafe {
                    metadata.push_debug_checked(meta);
                    let idx = metadata.len() - 1;
                    let meta_ptr = metadata.get_debug_checked(idx);
                    SlagConfig::new(NonNull::new_unchecked(meta_ptr as *mut _), 1 << 20)
                }
            };
            let small = Multiples::init(
                            Sixteen::VAL,
                            n_small_classes,
                            |size| ObjectAlloc::new(init_slag_config(size))
                        ).alloc_expect("out of memory");
            alloc_assert_eq!(min_medium_class, (small.max_key() + 1).next_power_of_two());
            let medium = PowersOfTwo::init(min_medium_class, n_medium_classes, |size: usize| {
                let target_size: usize = cmp::max(1, self.target_pipe_overhead / size);
                Lazy::new(PageFrontendConfig { 
                            size,
                            max_overhead: target_size,
                            pipe_size: self.small_pipe_size
                        })
            }).alloc_expect("out of memory");
            (small, medium)
        };


        let small_page_size = self.page_size;
        let alloc = MapAllocBuilder::default()
                        .obj_size(small_page_size)
                        .obj_align(small_page_size)
                        .build();
        let small_pages = PageCache::new(alloc, 8);

        let medium_page_size = self.max_object_size.next_power_of_two();
        let alloc = MapAllocBuilder::default()
                        .obj_size(medium_page_size)
                        .obj_align(medium_page_size)
                        .build();
        let medium_pages = PageSource::new(
                        self.large_obj_cutoff,
                        self.large_obj_target_size,
                        self.large_pipe_size,
                        alloc,
                     );
        alloc_debug_assert!(small.max_key().is_power_of_two());

        ElfMalloc {
            small,
            medium,
            small_pages,
            medium_pages,
            metadata: Arc::new(metadata),
        }
    }
}

pub use self::global::GlobalAlloc;

mod global {
    //! This module provides an interface to global instances of allocators in the parent module.
    //! Two interfaces are provided. All allocations from either of these allocators share the same
    //! global data-structures.
    //!
    //! # `GlobalAlloc`
    //!
    //! This allocator stores a single frontend per thread in thread-local storage (TLS). It
    //! therefore allows different data-structures to share a single frontend. In experiments with
    //! a custom `Vec` implementation, this leads to serious performance wins.
    
    use std::cell::UnsafeCell;

    use object::ElfBuilder;
    use super::*;

    lazy_static! {
        static ref GLOBAL_HANDLE: ElfMalloc = ElfBuilder::default()
                                                    .page_size(16 << 10)
                                                    .build_elfmalloc();
    }

    alloc_thread_local! {
        static LOCAL_CACHE: UnsafeCell<ElfMalloc> =  UnsafeCell::new(GLOBAL_HANDLE.clone());
    }

    fn with_local_or_clone<F, R>(f: F) -> R
        where F: Fn(&UnsafeCell<ElfMalloc>) -> R
    {
        unsafe {
            alloc_tls_fast_with!(LOCAL_CACHE, h, { f(h) })
                .unwrap_or_else(|| f(&UnsafeCell::new(GLOBAL_HANDLE.clone())))
        }
    }

    /// A ZST for routing allocations through a global singleton.
    #[derive(Clone)]
    pub struct GlobalAlloc;

    unsafe impl Alloc for GlobalAlloc {
        unsafe fn alloc(&mut self, l: Layout) -> Result<*mut u8, AllocErr> {
            with_local_or_clone(|alloc| (*alloc.get()).alloc((&l).clone()))
        }

        unsafe fn dealloc(&mut self, p: *mut u8, l: Layout) {
            with_local_or_clone(|alloc| (*alloc.get()).dealloc(p, (&l).clone()))
        }

        fn usable_size(&self, l: &Layout) -> (usize, usize) {
            unsafe { with_local_or_clone(|alloc| (*alloc.get()).usable_size((&l).clone())) }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::mem;
    use std::ptr;
    use std::thread;

    use alloc_fmt::AllocUnwrap;

    #[test]
    fn basic_alloc_functionality() {
        let word_size = mem::size_of::<usize>();
        let mut alloc = ElfMallocBuilder::default().build();
        let layouts: Vec<_> = (word_size..(8 << 10))
            .map(|size| {
                Layout::from_size_align(size * 128, word_size).alloc_unwrap()
            })
            .collect();
        unsafe {
            let mut ptrs = Vec::new();
            for l in &layouts {
                let p = alloc.alloc(l.clone()).alloc_expect("out of memory") as *mut usize;
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
        let alloc = ElfMallocBuilder::default().build();

        let mut threads = Vec::new();
        for _ in 0..N_THREADS {
            let mut my_alloc = alloc.clone();
            let my_layouts = layouts.clone();
            threads.push(thread::spawn(move || unsafe {
                for _ in 0..2 {
                    let mut ptrs = Vec::new();
                    for l in &my_layouts {
                        let p = my_alloc.alloc(l.clone()).expect("out of memory") as
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
    #[cfg_attr(feature = "low-memory-tests", ignore)]
    fn many_threads_many_sizes() {
        let word_size = mem::size_of::<usize>();
        multi_threaded_alloc_test(
            (word_size..2 << 10)
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
