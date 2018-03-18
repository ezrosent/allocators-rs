// Copyright 2018 the authors. See the 'Copyright and license' section of the
// README.md file at the top-level directory of this repository.
//
// Licensed under the Apache License, Version 2.0 (the LICENSE-APACHE file) or
// the MIT license (the LICENSE-MIT file) at your option. This file may not be
// copied, modified, or distributed except according to those terms.

use alloc::allocator::Layout;
use crossbeam_epoch::{Collector, Handle};
use mmap_alloc::{MapAlloc, MapAllocBuilder};
use object_alloc::{ObjectAlloc, UntypedObjectAlloc};
use std::ptr::NonNull;

use backing::PageCache;
use frontends::{Frontend, MagazineCache};
use slag::SlagAllocator;
use util::TryClone;

pub struct ElfUntypedObjectAlloc {
    inner: ElfUntypedObjectAllocInner<MagazineCache>,
}

impl ElfUntypedObjectAlloc {
    pub fn new(layout: Layout) -> ElfUntypedObjectAlloc {
        ElfUntypedObjectAlloc {
            inner: ElfUntypedObjectAllocInner::new(layout).expect("out of memory")
        }
    }
}

unsafe impl UntypedObjectAlloc for ElfUntypedObjectAlloc {
    #[inline]
    fn layout(&self) -> Layout {
        self.inner.layout()
    }

    #[inline]
    unsafe fn alloc(&mut self) -> Option<NonNull<u8>> {
        self.inner.alloc()
    }

    #[inline]
    unsafe fn dealloc(&mut self, ptr: NonNull<u8>) {
        self.inner.dealloc(ptr);
    }
}

impl Clone for ElfUntypedObjectAlloc {
    fn clone(&self) -> Self {
        ElfUntypedObjectAlloc { inner: self.inner.try_clone().expect("out of memory") }
    }
}

struct ElfUntypedObjectAllocInner<F: Frontend> {
    pages: PageCache<MapAlloc>,
    inner: F,
    layout: Layout,
    handle: Handle,
}

impl<F: Frontend> ElfUntypedObjectAllocInner<F> {
    fn new(layout: Layout) -> Option<ElfUntypedObjectAllocInner<F>> {
        alloc_assert!(layout.size() > 0);
        let page_size = unimplemented!();
        let alloc = MapAllocBuilder::default()
            .obj_size(page_size)
            .obj_align(page_size)
            .commit(cfg!(windows))
            .build();
        let mut pages = PageCache::new(alloc, 1 << 20);
        let handle = Collector::new().handle();
        let slag = SlagAllocator::new(
                        &mut pages,
                        unimplemented!(), //max_objects,
                        unimplemented!(), //mem::size_of::<T>(),
                        0,
                        unimplemented!(), //cutoff_factor,
                        unimplemented!(), //eager_decommit
                    )?;
        let inner = F::new(&handle, &mut pages, slag)?;
        Some(ElfUntypedObjectAllocInner { pages, inner, layout, handle })
    }
}

unsafe impl<F: Frontend> UntypedObjectAlloc for ElfUntypedObjectAllocInner<F> {
    fn layout(&self) -> Layout {
        self.layout.clone()
    }

    unsafe fn alloc(&mut self) -> Option<NonNull<u8>> {
        self.inner.alloc_with(&self.handle, &mut self.pages)
    }

    unsafe fn dealloc(&mut self, ptr: NonNull<u8>) {
        self.inner.dealloc_with(&self.handle, &mut self.pages, ptr);
    }
}

impl<F: Frontend> TryClone for ElfUntypedObjectAllocInner<F> {
    fn try_clone(&self) -> Option<Self> {
        let mut pages = self.pages.try_clone()?;
        let handle = self.handle.try_clone()?;
        let inner = unsafe { self.inner.try_clone_with(&handle, &mut pages)? };
        let layout = self.layout.clone();
        Some(ElfUntypedObjectAllocInner { pages, inner, layout, handle })
    }
}

impl<F: Frontend> Drop for ElfUntypedObjectAllocInner<F> {
    fn drop(&mut self) {
        unsafe { self.inner.pre_drop(&self.handle, &mut self.pages) };
    }
}

// #[derive(Clone)]
// pub struct ElfUntypedObjectAllocInner<P: UntypedObjectAlloc> {
//     pages: P,
//     elf: ElfSizeClass, // TODO: rename?
//     layout: Layout,
// }

// impl<P: UntypedObjectAlloc> ElfUntypedObjectAllocInner<P> {
//     pub fn new(layout: Layout, pages: P) -> ElfUntypedObjectAlloc {
//         ElfUntypedObjectAllocInner {
//             pages,
//             elf: unimplemented!(),
//             layout,
//         }
//     }

//     // pub(crate) fn with_page_size(layout: Layout, page_size: usize) -> ElfUntypedObjectAlloc {
//     //     let alloc = MapAllocBuilder::default()
//     //         .obj_size(page_size)
//     //         .obj_align(page_size)
//     //         .commit(cfg!(windows))
//     //         .build();
//     //     ElfUntypedObjectAlloc{
//     //         pages: PageCache::new(alloc, 1 << 20),
//     //         elf: unimplemented!(),
//     //         layout
//     //     } 
//     // }
// }

// impl ElfUntypedObjectAllocInner<MarkedAlloc<PageCache<MapAlloc>>> {
//     pub(crate) fn new_for_elfmalloc(layout: Layout, page_size: usize, ty: AllocType) -> ElfUntypedObjectAllocInner<MarkedAlloc<PageCache<MapAlloc>>> {
//         let alloc = MapAllocBuilder::default()
//             .obj_size(page_size)
//             .obj_align(page_size)
//             .commit(cfg!(windows))
//             .build();
//         Self::new(layout, MarkedAlloc::new(PageCache::new(alloc, 1 << 20), ty))
//     }
// }

// unsafe impl<P: UntypedObjectAlloc> UntypedObjectAlloc for ElfUntypedObjectAllocInner<P> {
//     fn layout(&self) -> Layout {
//         self.layout.clone()
//     }

//     unsafe fn alloc(&mut self) -> Result<NonNull<u8>, Exhausted> {
//         self.elf.alloc(&mut self.pages).map(NonNull::cast).ok_or(Exhausted)
//     }

//     unsafe fn dealloc(&mut self, ptr: NonNull<u8>) {
//         self.elf.dealloc(&mut self.pages, ptr);
//     }
// }



// // TODO: The point of having alloc and dealloc take a page allocator as an argument
// // was to avoid having to copy the page allocators a lot and waste space.
// // Is this a concern? If so, we'll need to modify AllocMap to be able to ask for
// // the right page allocator. If not, we can just keep doing what we're doing and
// // store a copy of the page allocator in each size class.
// #[derive(Clone)]
// pub struct ElfSizeClass {

// }

// impl ElfSizeClass {
//     fn alloc<P: UntypedObjectAlloc>(&mut self, pages: &mut P) -> Option<NonNull<u8>> {
//         unimplemented!()
//     }

//     fn dealloc<P: UntypedObjectAlloc>(&mut self, pages: &mut P, ptr: NonNull<u8>) {
//         unimplemented!()
//     }
// }