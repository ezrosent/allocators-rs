// Copyright 2017-2018 the authors. See the 'Copyright and license' section of the
// README.md file at the top-level directory of this repository.
//
// Licensed under the Apache License, Version 2.0 (the LICENSE-APACHE file) or
// the MIT license (the LICENSE-MIT file) at your option. This file may not be
// copied, modified, or distributed except according to those terms.

//! Implementation of traditional `malloc`-style allocator routines based off of the `Slag`
//! allocator design.
//!
//! The primary use of this module is to provide the rudaments of a `malloc`-compatible global
//! allocator that can be used from C/C++ and Rust programs alike. The `elfc` crate that wraps
//! this one exposes such an interface. It is currently possible to use this module as a Rust
//! library, though we do not recommend it.
//!
//! # Using this Allocator from Rust
//!
//! We currently rely on some global allocator (bsalloc) to be running to service normal heap
//! allocations. As a result, this allocator cannot be used as a global allocator via the
//! `#[global_allocator]` attribute. Currently the only way around this is to use the `System`
//! allocator along with `libelfc` from the `elfc` crate loaded with `LD_PRELOAD`.
//!
//! It is also possible to use this allocator using a `Clone`-based API. As alluded to elsewhere,
//! the allocator is thread-safe and any handle on the allocator can be used to free a pointer from
//! any other handle in any other thread. If you `free` a pointer `alloc`-ed by another
//! `DynamicAllocator`, bad things will happen.
//!
//! ```rust,ignore
//! // all calls to `alloc` and `free` are unsafe
//! let mut elf = DynamicAllocator::new();
//! let ptr = elf.alloc(16) as *mut [u8; 16];
//! let mut elf_clone = elf.clone();
//! let res = thread::spawn(move || {
//!     elf_clone.alloc(24) as *mut [u8; 24]
//! }).join().unwrap();
//! elf.free(res);
//! elf.free(ptr);
//! ```
//!
//! This is probably a more limited use-case until custom allocators have better support in the
//! Rust ecosystem. Even then, we suspect most programmers using a non-global allocator will
//! instead want something more specialized, such as the `LocalAllocator` and `MagazineAllocator`
//! object-specific allocators.

use std::cmp;
use std::intrinsics::likely;
use std::mem;
use std::ptr::{self, NonNull};

use crossbeam_epoch::{Collector, Handle};
use mmap_alloc::{MapAlloc, MapAllocBuilder};
use object_alloc::UntypedObjectAlloc;

// One of MagazineCache and LocalCache is unused, depending on whether the 'local_cache' feature is
// enabled.
// use super::sources::{MemorySource, MmapSource};
use alloc_map::{AllocMap, TieredSizeClasses};
use alloc_type::AllocType;
use backing::{MarkedAlloc, PageCache};
use frontends::MagazineCache;
use slag::{DirtyFn, Slag};
#[allow(unused_imports)]
use util::{mmap, AllocWith, Const, Lazy, TryClone, TypedArray};

struct Sixteen;

impl Const<usize> for Sixteen {
    const VAL: usize = 16;
}

pub(crate) mod global {
    //! A global malloc-style interface to interact with a `DynamicAllocator`. All of these
    //! structures are lazily initailized.
    //!
    //! One could be forgiven for thinking that this could work by simply using a global
    //! `lazy_static`-managed instance of a `DynamicAllocator` and then using thread-local storage
    //! (TLS) to store handles to this global instance. While this is essentially the architecture
    //! we use, a number of hacks have been added to ensure correctness.
    //!
    //! ## Recursive `malloc` calls
    //!
    //! When used as a standard `malloc` implementation through the `elfc` crate via `LD_PRELOAD`,
    //! all calls to `malloc` and related functions will be routed through this module. The only
    //! problem is that the code that enqueues destructors for pthread TSD calls `calloc`; this
    //! causes all such calls to stack overflow.
    //!
    //! In order to avoid thisi problem, we use the `alloc-tls` crate, which can detect this
    //! recursion. When such recursion is detected, we fall back on a slow path of creating a new
    //! one-time-use instance of the value that is normally stored in thread-local storage in
    //! order to perform the operation and then discard it.
    use std::ptr::NonNull;
    use std::cell::UnsafeCell;
    use std::mem;
    #[allow(unused_imports)]
    use std::sync::atomic::{AtomicUsize, Ordering};

    use mmap_alloc::{MapAlloc, MapAllocBuilder};
    use object_alloc::UntypedObjectAlloc;

    use alloc_map::{AllocMap, TieredSizeClasses};
    use backing::PageCache;
    #[allow(unused_imports)]
    use super::{AllocType, DynamicAllocator, DirtyFn, ElfMalloc, ObjectAlloc,
                Sixteen, TypedArray, get_type};
    use util::TryClone;

    unsafe fn dirty_slag(mem: NonNull<u8>) {
        trace!("dirtying {:?}", mem);
        let usable_size = 32 << 10;
        let base_page = 4096;
        let mem = mem.as_ptr();
        let mut cur_addr = mem.offset(base_page);
        while cur_addr < mem.offset(usable_size) {
            cur_addr = cur_addr.offset(base_page);
            (*(cur_addr as *mut AtomicUsize)).compare_and_swap(0, 1, Ordering::Relaxed);
        }
    }

    /// A wrapper like `DynamicAllocator` in the parent module.
    ///
    /// The reason we have a wrapper is for this module's custom `Drop` implementation, mentioned
    /// in the module documentation.
    struct GlobalAllocator {
        alloc: ElfMalloc<PageCache<MapAlloc>>,
        // In some rare cases, we've observed that a thread-local GlobalAllocator is spuriously
        // dropped twice. Until we figure out why and fix it, we just detect when it's happening
        // and make the second drop call a no-op.
        dropped: bool,
    }

    unsafe impl Send for GlobalAllocator {}

    impl TryClone for GlobalAllocator {
        fn try_clone(&self) -> Option<Self> {
            Some(GlobalAllocator {
                alloc: self.alloc.try_clone()?,
                dropped: false,
            })
        }
    }

    /// The type of the global instance of the allocator.
    ///
    /// This is used to create handles for TLS-stored `GlobalAllocator`s.
    struct GlobalAllocProvider {
        inner: Option<ElfMalloc<PageCache<MapAlloc>>>,
    }

    // We need sync to have the global allocator reference live for new threads to clone. This is
    // safe only because ElfMalloc (and PageAlloc, and TieredSizeClasses) have thread-safe clone
    // methods.
    unsafe impl Sync for GlobalAllocProvider {}
    unsafe impl Send for GlobalAllocProvider {}
    impl GlobalAllocProvider {
        fn new() -> Option<GlobalAllocProvider> {
            let alloc = MapAllocBuilder::default()
                            .obj_size(super::ELFMALLOC_PAGE_SIZE)
                            .obj_align(super::ELFMALLOC_PAGE_SIZE)
                            .build();
            let alloc = PageCache::new(alloc, 8);
            Some(GlobalAllocProvider { inner: Some(ElfMalloc::new(alloc.try_clone()?, alloc)) })
        }
    }

    impl Drop for GlobalAllocator {
        fn drop(&mut self) {
            // XXX: Why this check?
            //
            // We have found that for some reason, this destructor can be called more than once on
            // the same value. This could be a peculiarity of the TLS implementation, or it could
            // be a bug in the code here. Regardless; without this check there are some cases in
            // which this benchmark drops Arc-backed data-structures multiple times, leading to
            // segfaults.
            if self.dropped {
                alloc_eprintln!("{:?} dropped twice!", self as *const _);
                return;
            }
            self.dropped = true;
        }
    }

    pub unsafe fn get_layout(item: NonNull<u8>) -> (usize /* size */, usize /* alignment */) {
        let page_size = match get_type(item) {
            // TODO(ezrosent): this duplicates some work..
            AllocType::SmallSlag | AllocType::Large => {
                with_local_or_clone(|h| {
                    (*h.get())
                        .alloc
                        .small_pages
                        .layout()
                        .size()
                })
            }
            AllocType::BigSlag => {
                with_local_or_clone(|h| {
                    (*h.get())
                        .alloc
                        .medium_pages
                        .layout()
                        .size()
                })
            }
        };
        super::elfmalloc_get_layout(page_size, item)
    }

    fn new_handle() -> Option<GlobalAllocator> {
        Some(GlobalAllocator {
            alloc: ELF_HEAP.inner.as_ref().expect("heap uninitialized").try_clone()?,
            dropped: false,
        })
    }

    impl Drop for GlobalAllocProvider {
        fn drop(&mut self) {
            mem::forget(self.inner.take());
        }
    }

    lazy_static! { static ref ELF_HEAP: GlobalAllocProvider = GlobalAllocProvider::new().expect("out of memory"); }
    alloc_thread_local!{ static LOCAL_ELF_HEAP: UnsafeCell<GlobalAllocator> = UnsafeCell::new(new_handle().expect("out of memory")); }

    fn with_local_or_clone<F, R>(f: F) -> R
        where F: Fn(&UnsafeCell<GlobalAllocator>) -> R
    {
        unsafe {
            alloc_tls_fast_with!(LOCAL_ELF_HEAP, h, { f(h) })
                .unwrap_or_else(|| f(&UnsafeCell::new(new_handle().expect("out of memory"))))
        }
    }

    pub unsafe fn alloc(size: usize) -> Option<NonNull<u8>> {
        alloc_tls_fast_with!(LOCAL_ELF_HEAP, h, { (*h.get()).alloc.alloc(size) })
            .unwrap_or_else(|| super::large_alloc::alloc(size))
    }

    pub unsafe fn realloc(item: NonNull<u8>, new_size: usize) -> Option<NonNull<u8>> {
        aligned_realloc(item, new_size, mem::size_of::<usize>())
    }

    pub unsafe fn aligned_realloc(item: NonNull<u8>, new_size: usize, new_alignment: usize) -> Option<NonNull<u8>> {
        with_local_or_clone(|h| (*h.get()).alloc.realloc(item, new_size, new_alignment))
    }

    pub unsafe fn dealloc(item: NonNull<u8>) {
        with_local_or_clone(|h| (*h.get()).alloc.dealloc(item));
    }
}

// impl Drop for DynamicAllocator {
//     fn drop(&mut self) {
//         self.0.allocs.foreach(|x| unsafe { ptr::drop_in_place(x) });
//         unsafe {
//             self.0.allocs.medium_objs.classes.destroy();
//             self.0.allocs.small_objs.classes.destroy();
//             #[cfg(any(not(feature = "c-api"),
//                         not(any(target_os = "macos",
//                                     all(windows, target_pointer_width = "64")))))]
//             ptr::write(&mut self.0.allocs.word_objs, None);
//         }
//     }
// }

/// A Dynamic memory allocator, instantiated with sane defaults for various `ElfMalloc` type
/// parameters.
pub struct DynamicAllocator(ElfMalloc<PageCache<MapAlloc>>);

unsafe impl Send for DynamicAllocator {}

impl DynamicAllocator {
    pub fn new() -> Option<Self> {
        let alloc = MapAllocBuilder::default()
                            .obj_size(ELFMALLOC_PAGE_SIZE)
                            .obj_align(ELFMALLOC_PAGE_SIZE)
                            .build();
        let alloc = PageCache::new(alloc, 8);
        Some(DynamicAllocator(ElfMalloc::new(alloc.try_clone()?, alloc)))
    }
    pub unsafe fn alloc(&mut self, size: usize) -> Option<NonNull<u8>> {
        self.0.alloc(size)
    }
    pub unsafe fn dealloc(&mut self, item: NonNull<u8>) {
        self.0.dealloc(item)
    }

    pub unsafe fn realloc(&mut self, item: NonNull<u8>, new_size: usize) -> Option<NonNull<u8>> {
        self.0.realloc(item, new_size, mem::size_of::<usize>())
    }

    pub unsafe fn aligned_realloc(
        &mut self,
        item: NonNull<u8>,
        new_size: usize,
        new_alignment: usize,
    ) -> Option<NonNull<u8>> {
        self.0.realloc(item, new_size, new_alignment)
    }
}

impl Clone for DynamicAllocator {
    fn clone(&self) -> Self {
        DynamicAllocator(self.0.try_clone().expect("out of memory"))
    }
}

// Frontends are currently feature-gated in the following fashion:

#[cfg(not(feature = "local_cache"))]
type Inner = MagazineCache;
#[cfg(feature = "local_cache")]
type Inner = LocalCache;

#[cfg(not(feature = "magazine_layer"))]
pub(crate) type ObjectAlloc = Lazy<Inner>;
#[cfg(feature = "magazine_layer")]
pub(crate) type ObjectAlloc = Lazy<DepotCache<Inner>>;

/// A Dynamic memory allocator, parmetrized on a particular `ObjectAlloc`, `CourseAllocator` and
/// `AllocMap`.
///
/// `ElfMalloc` encapsulates the logic of constructing and selecting object classes, as well as
/// delgating to the `large_alloc` module for large allocations. Most of the logic occurs in its
/// type parameters.
struct ElfMalloc<P: UntypedObjectAlloc> {
    /// A map of size classes. Each size class corresponds to a single object
    /// size and contains a single ObjectAlloc instance.
    allocs: TieredSizeClasses<ObjectAlloc, MarkedAlloc<P>, MarkedAlloc<P>, Sixteen>,
    handle: Handle,

    start_from: usize,
    n_classes: usize,
}

impl Default for DynamicAllocator {
    fn default() -> Self {
        Self::new().expect("out of memory")
    }
}

// TODO(ezrosent): move this to a type parameter when const generics are in.
pub(crate) const ELFMALLOC_PAGE_SIZE: usize = 2 << 20;
pub(crate) const ELFMALLOC_SMALL_PAGE_SIZE: usize = 256 << 10;
pub(crate) const ELFMALLOC_SMALL_CUTOFF: usize = ELFMALLOC_SMALL_PAGE_SIZE / 4;

impl<P: UntypedObjectAlloc> ElfMalloc<P> {
    fn new(small_pages: P, medium_pages: P) -> Self {
        Self::new_internal(0.6, small_pages, medium_pages, 8, 25)

        // let pa_large = PageAlloc::new(ELFMALLOC_PAGE_SIZE, 1 << 20, 8, AllocType::BigSlag);
        // The small pages are allocated in groups where the first page is aligned to
        // ELFMALLOC_PAGE_SIZE; this page will be stamped with AllocType::SmallSlag, allowing type
        // lookups to work as expected.
        // let pa_small = PageAlloc::new_aligned(
        //     ELFMALLOC_SMALL_PAGE_SIZE,
        //     1 << 20,
        //     8,
        //     ELFMALLOC_PAGE_SIZE,
        //     AllocType::SmallSlag,
        // );
        // Self::new_internal(0.6, pa_small, pa_large, 8, 25)
    }

    fn new_internal(
        cutoff_factor: f64,
        small_pages: P,
        medium_pages: P,
        start_from: usize,
        n_classes: usize,
    ) -> Self {
        unimplemented!()

        // use self::mmap::map;

        // let small_pages = MarkedAlloc::new(small_pages, AllocType::SmallSlag);
        // let medium_pages = MarkedAlloc::new(medium_pages, AllocType::BigSlag);
        
        // let mut meta_pointer = map(mem::size_of::<Metadata>() * n_classes) as *mut Metadata;
        // let small_page_size = small_pages.layout().size();
        // let am = AM::init(start_from, n_classes, |size: usize| {
        //     let (page_size, ty) = if size < ELFMALLOC_SMALL_CUTOFF {
        //         (small_page_size, AllocType::SmallSlag)
        //     } else {
        //         (medium_pages.layout().size(), AllocType::BigSlag)
        //     };
        //     let m_ptr = meta_pointer;
        //     unsafe {
        //         meta_pointer = meta_pointer.offset(1);
        //         ptr::write(
        //             m_ptr,
        //             compute_metadata(
        //                 size,
        //                 pa.backing_memory().page_size(),
        //                 0,
        //                 cutoff_factor,
        //                 u_size,
        //                 ty,
        //             ),
        //         );
        //     }
        //     let clean = PageCleanup::new(pa.backing_memory().page_size());
        //     // TODO(ezrosent); new_size(8) is a good default, but a better one would take
        //     // num_cpus::get() into account when picking this size, as in principle this will run
        //     // into scaling limits at some point.
        //     let params = (
        //         m_ptr,
        //         1 << 20,
        //         pa,
        //         RevocablePipe::new_size_cleanup(16, clean),
        //     );
        //     #[cfg(not(feature = "magazine_layer"))]
        //     {
        //         ObjectAlloc::new(params)
        //     }
        //     #[cfg(feature = "magazine_layer")]
        //     {
        //         ObjectAlloc::new((params, Depot::default()))
        //     }
        // });
        // let max_size = am.max_key();
        // ElfMalloc {
        //     small_pages: small_pages,
        //     medium_pages: medium_pages,
        //     allocs: am,
        //     max_size: max_size,
        //     start_from: start_from,
        //     n_classes: n_classes,
        // }
    }

    #[inline]
    unsafe fn get_page_size(&self, item: NonNull<u8>) -> Option<usize> {
        // We have carfeully orchestrated things so that allocation sizes above the cutoff are
        // aligned to at least that cutoff:
        // - Medium objects are powers of two, all of which are aligned to their size.
        // - Large objects are allocated using an MmapSource with page size equivalent to the
        //   cutoff.
        // As a result, we do not have to dereference an extra pointer for small objects that are
        // not aligned to the small cutoff (this is going to be most of them). This netted
        // small-but-noticeable performance gains.
        if (item.as_ptr() as usize) % ELFMALLOC_SMALL_CUTOFF != 0 {
            return Some(ELFMALLOC_SMALL_PAGE_SIZE);
        }
        match get_type(item) {
            AllocType::SmallSlag => {
                alloc_debug_assert_eq!(self.small_pages.layout().size(), ELFMALLOC_SMALL_PAGE_SIZE);
                Some(ELFMALLOC_SMALL_PAGE_SIZE)
            },
            AllocType::BigSlag => {
                alloc_debug_assert_eq!(self.medium_pages.layout().size(), ELFMALLOC_PAGE_SIZE);
                Some(ELFMALLOC_PAGE_SIZE)
            },
            AllocType::Large => None,
        }
    }

    unsafe fn alloc(&mut self, bytes: usize) -> Option<NonNull<u8>> {
        if likely(bytes <= self.allocs.max_size()) {
            self.allocs.with(
                bytes,
                |frontend, pages| frontend.alloc_with(&self.handle, pages),
                |frontend, pages| frontend.alloc_with(&self.handle, pages),
            )
            // let pages = if likely(bytes <= self.max_small) {
            //     &mut self.small_pages
            // } else {
            //     &mut self.medium_pages
            // };
            // self.allocs.get_mut(bytes).alloc(&self.handle, pages)
        } else {
            large_alloc::alloc(bytes)
        }
    }

    unsafe fn realloc(
        &mut self,
        item: NonNull<u8>,
        mut new_size: usize,
        new_alignment: usize,
    ) -> Option<NonNull<u8>> {
        let (old_size, old_alignment) = global::get_layout(item);
        if old_alignment >= new_alignment && old_size >= new_size {
            return Some(item);
        }
        if new_alignment > mem::size_of::<usize>() {
            new_size = new_size.next_power_of_two();
        }
        let new_mem = self.alloc(new_size)?;
        ptr::copy_nonoverlapping(item.as_ptr(), new_mem.as_ptr(), ::std::cmp::min(old_size, new_size));
        self.dealloc(item);
        #[cfg(debug_assertions)]
        {
            let (size, _) = global::get_layout(new_mem);
            alloc_debug_assert!(new_size <= size, "Realloc for {} got memory with size {}", new_size, size);
        }
        Some(new_mem)
    }

    unsafe fn dealloc(&mut self, item: NonNull<u8>) {
        match self.get_page_size(item) {
            Some(page_size) => {
                let object_size = Slag::find(item, page_size).as_ref().get_metadata().object_size;
                self.allocs.with(
                    object_size,
                    |frontend, pages| frontend.dealloc_with(&self.handle, pages, item),
                    |frontend, pages| frontend.dealloc_with(&self.handle, pages, item),
                )
                // let pages = if likely(object_size <= self.max_small) {
                //     &mut self.small_pages
                // } else {
                //     &mut self.medium_pages
                // };
                // self.allocs.get_mut(object_size).dealloc(
                //     &self.handle,
                //     pages,
                //     item,
                // )
            }
            None => large_alloc::dealloc(item),
        };
    }
}

impl<P: UntypedObjectAlloc> TryClone for ElfMalloc<P> {
    fn try_clone(&self) -> Option<Self> {
        // let new_map = ;
        // AM::init(self.start_from, self.n_classes, |size: usize| unsafe {
        //     // TODO: AllocMaps need to be able to roll back (and call pre_drop)
        //     // on OOM.
        //     self.allocs.get(size).try_clone().expect("out of memory")
        // });
        Some(ElfMalloc {
            handle: self.handle.clone(),
            allocs: self.allocs.try_clone()?,
            start_from: self.start_from,
            n_classes: self.n_classes,
        })
    }
}

#[inline(always)]
unsafe fn round_to_page<T>(item: *mut T) -> *mut T {
    ((item as usize) & !(ELFMALLOC_PAGE_SIZE - 1)) as *mut T
}

/// We ensure that for every pointer returned from a call to `alloc`, rounding that pointer down to
/// a 2MiB boundary yields the location of an `AllocType`. This is enforced separately in the
/// `PageAlloc` code, the `large_alloc` code, and the `Slag` code.
///
/// All of this allows us to run elfmalloc with a full malloc-style interface without resorting to
/// any sort of global ownership check on the underlying `MemorySource`. This method thus breaks
/// our dependency on the `Creek`.
#[inline(always)]
unsafe fn get_type(item: NonNull<u8>) -> AllocType {
    *round_to_page(item.as_ptr().offset(-1) as *mut AllocType)
}

unsafe fn elfmalloc_get_layout(page_size: usize, item: NonNull<u8>) -> (usize, usize) {
    match get_type(item) {
        AllocType::SmallSlag | AllocType::BigSlag => {
            let object_size = Slag::find(item, page_size).as_ref().get_metadata().object_size;
            (
                object_size,
                if object_size.is_power_of_two() {
                    object_size
                } else {
                    mem::size_of::<usize>()
                },
            )
        }
        AllocType::Large => (large_alloc::get_size(item), mmap::page_size()),
    }
}

mod large_alloc {
    //! This module governs "large" allocations that are beyond the size of the largest size class
    //! of a dynamic allocator.
    //!
    //! Large allocations are implemented by mapping a region of memory of the indicated size, with
    //! an additional page of padding to store the size information.
    #[cfg(test)]
    use std::collections::HashMap;
    #[cfg(test)]
    use std::cell::RefCell;
    use std::cmp;
    use std::ptr::{self, NonNull};

    use alloc::allocator::{Alloc, Layout};
    use mmap_alloc::MapAlloc;

    // use super::super::sources::{MemorySource, MmapSource};
    use super::{ELFMALLOC_PAGE_SIZE, ELFMALLOC_SMALL_CUTOFF, round_to_page};
    use alloc_type::AllocType;

    // For debugging, we keep around a thread-local map of pointers to lengths. This helps us
    // scrutinize whether various header data is getting propagated correctly.
    #[cfg(test)]
    thread_local! {
        pub static SEEN_PTRS: RefCell<HashMap<*mut u8, usize>> = RefCell::new(HashMap::new());
    }
    use super::mmap::unmap;
    #[cfg(debug_assertions)]
    use super::mmap::page_size;

    #[repr(C)]
    #[derive(Copy, Clone)]
    pub struct AllocInfo {
        pub ty: AllocType,
        base: NonNull<u8>,
        region_size: usize,
    }

    pub unsafe fn alloc(size: usize) -> Option<NonNull<u8>> {
        // TODO(ezrosent) round up to page size
        let region_size = size + ELFMALLOC_PAGE_SIZE;
        // We need a pointer aligned to the SMALL_CUTOFF, so we use an `MmapSource` to map the
        // memory. See the comment in get_page_size.

        let layout = Layout::from_size_align(region_size + cmp::min(1, region_size % ELFMALLOC_SMALL_CUTOFF), ELFMALLOC_SMALL_CUTOFF).unwrap();
        let mem = NonNull::new_unchecked(MapAlloc::default().alloc(layout).ok()?);

        // let alloc = MapAllocBuilder::default().
        // let src = MmapSource::new(ELFMALLOC_SMALL_CUTOFF);
        // let n_pages = region_size / ELFMALLOC_SMALL_CUTOFF + cmp::min(1, region_size % ELFMALLOC_SMALL_CUTOFF);
        // let mem = src.carve(n_pages).expect("[lage_alloc::alloc] mmap failed");

        let res = NonNull::new_unchecked(mem.as_ptr().offset(ELFMALLOC_PAGE_SIZE as isize));
        let addr = get_commitment_mut(res);
        ptr::write(
            addr.as_ptr(),
            AllocInfo {
                ty: AllocType::Large,
                base: mem,
                region_size: region_size,
            },
        );


        // begin extra debugging information
        let m_usize = mem.as_ptr() as usize;
        alloc_debug_assert_eq!(m_usize % ELFMALLOC_SMALL_CUTOFF, 0);
        let upage: usize = 4096;
        alloc_debug_assert_eq!(m_usize % upage, 0);
        alloc_debug_assert_eq!(res.as_ptr() as usize % upage, 0);
        alloc_debug_assert_eq!(get_commitment(res), (size + ELFMALLOC_PAGE_SIZE, mem));
        #[cfg(test)] SEEN_PTRS.try_with(|hs| hs.borrow_mut().insert(mem, region_size));
        // end extra debugging information

        Some(res)
    }

    pub unsafe fn dealloc(item: NonNull<u8>) {
        let (size, base_ptr) = get_commitment(item);
        trace!("size={}, base_ptr={:?}", size, base_ptr);
        // begin extra debugging information:
        #[cfg(debug_assertions)]
        {
            ptr::write_volatile(item.as_ptr(), 10);
            alloc_debug_assert_eq!(
                base_ptr.as_ptr() as usize % page_size(),
                0,
                "base_ptr ({:?}) not a multiple of the page size ({})",
                base_ptr,
                page_size()
            );
        }
        #[cfg(test)]
        {
            SEEN_PTRS.try_with(|hm| {
                let mut hmap = hm.borrow_mut();
                {
                    if let Some(len) = hmap.get(&base_ptr) {
                        alloc_assert_eq!(*len, size);
                    }
                }
                hmap.remove(&base_ptr);
            });
        }
        // end extra debugging information
        unmap(base_ptr.as_ptr(), size);
    }

    pub unsafe fn get_size(item: NonNull<u8>) -> usize {
        let (size, _) = get_commitment(item);
        size - ELFMALLOC_PAGE_SIZE
    }

    unsafe fn get_commitment(item: NonNull<u8>) -> (usize, NonNull<u8>) {
        let meta_addr = get_commitment_mut(item);
        let base_ptr = meta_addr.as_ref().base;
        let size = meta_addr.as_ref().region_size;
        (size, base_ptr)
    }

    pub unsafe fn get_commitment_mut(item: NonNull<u8>) -> NonNull<AllocInfo> {
        NonNull::new_unchecked(round_to_page(item.as_ptr().offset(-1) as *mut _))
    }
}

#[cfg(test)]
mod tests {
    extern crate env_logger;
    use super::*;
    use std::ptr::{write_bytes, write_volatile};


    #[test]
    fn layout_lookup() {
        fn test_and_free<F: Fn(usize, usize)>(inp: usize, tester: F) {
            unsafe {
                let obj = global::alloc(inp);
                let (size, align) = global::get_layout(obj);
                tester(size, align);
                global::dealloc(obj);
            }
        }

        test_and_free(8, |size, align| {
            alloc_assert!(size >= 8);
            alloc_assert!(align >= 8);
        });
        test_and_free(24, |size, align| {
            alloc_assert!(size >= 24);
            alloc_assert!(align >= 8);
        });
        test_and_free(512, |size, align| {
            alloc_assert!(size >= 512);
            alloc_assert!(align >= 512);
        });
        test_and_free(4 << 20, |size, align| {
            alloc_assert_eq!((size, align), (4 << 20, mmap::page_size()))
        });
    }

    #[test]
    fn general_alloc_basic_global_single_threaded() {
        let _ = env_logger::init();
        for size in ((1 << 13) - 8)..((1 << 13) + 1) {
            unsafe {
                let item = global::alloc(size * 8);
                write_volatile(item, 10);
                global::dealloc(item);
            }
        }
    }

    #[test]
    fn general_alloc_basic_clone_single_threaded() {
        let _ = env_logger::init();
        let da_c = DynamicAllocator::new();
        let mut da = da_c.clone();
        for size in ((1 << 13) - 8)..((1 << 13) + 1) {
            unsafe {
                let item = da.alloc(size * 8);
                write_volatile(item, 10);
                da.dealloc(item);
            }
        }
    }

    #[test]
    fn general_alloc_basic_global_many_threads() {
        let _ = env_logger::init();
        use std::thread;

        const N_THREADS: usize = 32;
        let mut threads = Vec::with_capacity(N_THREADS);
        for t in 0..N_THREADS {
            threads.push(
                thread::Builder::new()
                    .name(t.to_string())
                    .spawn(move || {
                        for size in 1..(1 << 13) {
                            // ((1 << 9) + 1)..((1 << 18) + 1) {
                            unsafe {
                                let item = global::alloc(size * 8);
                                write_volatile(item, 10);
                                global::dealloc(item);
                            }
                            if size * 8 >= (1 << 20) {
                                return;
                            }
                        }
                    })
                    .unwrap(),
            );
        }

        for t in threads {
            t.join().expect("threads should exit successfully")
        }
    }

    #[test]
    fn general_alloc_large_ws_global_many_threads() {
        let _ = env_logger::init();
        use std::thread;

        const N_THREADS: usize = 32;
        let mut threads = Vec::with_capacity(N_THREADS);
        for t in 0..N_THREADS {
            threads.push(
                thread::Builder::new()
                    .name(t.to_string())
                    .spawn(move || unsafe {
                        for _ in 0..2 {
                            let ptrs: Vec<*mut u8> =
                                (0..(1 << 20)).map(|_| global::alloc(8)).collect();
                            for p in ptrs {
                                global::dealloc(p);
                            }
                        }
                    })
                    .unwrap(),
            );
        }

        for t in threads {
            t.join().expect("threads should exit successfully")
        }
    }

    #[test]
    fn realloc_basic() {
        let _ = env_logger::init();
        use std::thread;
        const N_THREADS: usize = 8;
        let alloc = DynamicAllocator::new();
        let mut threads = Vec::with_capacity(N_THREADS);
        for t in 0..N_THREADS {
            let mut da = alloc.clone();
            threads.push(
                thread::Builder::new()
                    .name(t.to_string())
                    .spawn(move || for size in 1..(1 << 13) {
                        const N_ITERS: usize = 8;
                        let mut v1 = Vec::with_capacity(N_ITERS);
                        let alloc_size = size * 8;
                        if alloc_size >= (1 << 20) {
                            return;
                        }
                        unsafe {
                            for _ in 0..N_ITERS {
                                let item = da.alloc(alloc_size);
                                write_bytes(item, 0xFF, alloc_size);
                                v1.push(item);
                            }
                            for i in 0..N_ITERS {
                                let item = v1[i];
                                let new_item = da.aligned_realloc(
                                    item, alloc_size * 2, if size % 2 == 0 {
                                        8
                                    } else {
                                        (alloc_size * 2).next_power_of_two()
                                    });
                                write_bytes(new_item.offset(alloc_size as isize), 0xFE, alloc_size);
                                da.dealloc(new_item);
                            }
                        }
                    }).unwrap(),
            );
        }
        for t in threads {
            t.join().expect("threads should exit successfully")
        }
    }

    #[test]
    fn general_alloc_basic_clone_many_threads() {
        let _ = env_logger::init();
        use std::thread;

        const N_THREADS: usize = 32;
        let alloc = DynamicAllocator::new();
        let mut threads = Vec::with_capacity(N_THREADS);
        for t in 0..N_THREADS {
            let mut da = alloc.clone();
            threads.push(
                thread::Builder::new()
                    .name(t.to_string())
                    .spawn(move || {
                        for size in 1..(1 << 13) {
                            // ((1 << 9) + 1)..((1 << 18) + 1) {
                            unsafe {
                                let item = da.alloc(size * 8);
                                write_bytes(item, 0xFF, size * 8);
                                da.dealloc(item);
                            }
                            if size * 8 >= (1 << 20) {
                                return;
                            }
                        }
                    })
                    .unwrap(),
            );
        }

        for t in threads {
            t.join().expect("threads should exit successfully")
        }
    }

    #[test]
    fn all_sizes_one_thread() {
        let _ = env_logger::init();
        for size in 1..((1 << 21) + 1) {
            unsafe {
                let item = global::alloc(size);
                write_volatile(item, 10);
                global::dealloc(item);
                if size + 2 > 1 << 20 {
                    return;
                }
            }
        }
    }
}
