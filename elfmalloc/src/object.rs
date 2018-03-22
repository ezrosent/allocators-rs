// Copyright 2018 the authors. See the 'Copyright and license' section of the
// README.md file at the top-level directory of this repository.
//
// Licensed under the Apache License, Version 2.0 (the LICENSE-APACHE file) or
// the MIT license (the LICENSE-MIT file) at your option. This file may not be
// copied, modified, or distributed except according to those terms.

use std::cmp;
use std::ptr::NonNull;
use std::sync::Arc;

use alloc::allocator::Layout;
use alloc_fmt::AllocUnwrap;
use mmap_alloc::{MapAlloc, MapAllocBuilder};
use object_alloc::UntypedObjectAlloc;

use alloc_type::AllocType;
use backing::{GCAlloc, PageCache};
use frontends::{Frontend, LocalCache, MagazineCache};
use rust_alloc::{ElfMalloc, ElfMallocBuilder};
use slag::{compute_metadata, Metadata, SlagAllocator, SlagConfig};
use util::{AllocWith, ConfigBuild, LazyGuard, PreDrop, TryClone, TryCloneWith};

pub struct ElfUntypedObjectAlloc<F>
where
    F: Frontend<PageCache<MapAlloc>>,
{
    inner: ElfUntypedObjectAllocInner<F, PageCache<MapAlloc>>,
}

impl<F> ElfUntypedObjectAlloc<F>
where
    F: Frontend<PageCache<MapAlloc>>,
{
    pub fn new(layout: Layout) -> ElfUntypedObjectAlloc<F> {
        let builder = ElfBuilder::default();
        let pages = builder.mmap_page_cache(&layout);
        ElfUntypedObjectAlloc {
            inner: builder.build_untyped_inner(pages, layout).alloc_expect("out of memory"),
        }
    }
}

unsafe impl<F> UntypedObjectAlloc for ElfUntypedObjectAlloc<F>
where
    F: Frontend<PageCache<MapAlloc>>,
{
    #[inline]
    fn layout(&self) -> Layout {
        UntypedObjectAlloc::layout(&self.inner)
    }

    #[inline]
    unsafe fn alloc(&mut self) -> Option<NonNull<u8>> {
        UntypedObjectAlloc::alloc(&mut self.inner)
    }

    #[inline]
    unsafe fn dealloc(&mut self, ptr: NonNull<u8>) {
        UntypedObjectAlloc::dealloc(&mut self.inner, ptr);
    }
}

impl<F> Clone for ElfUntypedObjectAlloc<F>
where
    F: Frontend<PageCache<MapAlloc>>,
{
    fn clone(&self) -> Self {
        // TODO: Avoid leaking resources on failure (see issue #179)
        ElfUntypedObjectAlloc { inner: self.inner.try_clone().alloc_expect("out of memory") }
    }
}

pub struct ElfUntypedObjectAllocInner<F, B>
where
    F: PreDrop<B>,
{
    frontend: F,
    backing: B,
    layout: Layout,
    metadata: Arc<Metadata>,
}

unsafe impl<F, B> UntypedObjectAlloc for ElfUntypedObjectAllocInner<F, B>
where
    F: AllocWith<B> + PreDrop<B>,
{
    fn layout(&self) -> Layout {
        self.layout.clone()
    }

    unsafe fn alloc(&mut self) -> Option<NonNull<u8>> {
        self.frontend.alloc_with(&LazyGuard::new(), &mut self.backing)
    }

    unsafe fn dealloc(&mut self, ptr: NonNull<u8>) {
        self.frontend.dealloc_with(&LazyGuard::new(), &mut self.backing, ptr);
    }
}

// used in general::ElfMalloc
unsafe impl<F, B> GCAlloc for ElfUntypedObjectAllocInner<F, B>
where
    F: AllocWith<B> + PreDrop<B>,
{
    fn layout(&self) -> Layout {
        self.layout.clone()
    }

    unsafe fn alloc(&mut self, guard: &LazyGuard) -> Option<NonNull<u8>> {
        self.frontend.alloc_with(guard, &mut self.backing)
    }

    unsafe fn dealloc(&mut self, guard: &LazyGuard, ptr: NonNull<u8>) {
        self.frontend.dealloc_with(guard, &mut self.backing, ptr);
    }
}

impl<F, B> TryClone for ElfUntypedObjectAllocInner<F, B>
where
    F: TryCloneWith<B> + PreDrop<B>,
    B: TryClone,
{
    fn try_clone(&self) -> Option<Self> {
        // TODO: Avoid leaking resources on failure (see issue #179)
        let mut backing = self.backing.try_clone()?;
        let frontend = self.frontend.try_clone_with(&LazyGuard::new(), &mut backing)?;
        let layout = self.layout.clone();
        let metadata = self.metadata.clone();
        Some(ElfUntypedObjectAllocInner { backing, frontend, layout, metadata })
    }
}

impl<F, B> Drop for ElfUntypedObjectAllocInner<F, B>
where
    F: PreDrop<B>,
{
    fn drop(&mut self) {
        unsafe { self.frontend.pre_drop(&LazyGuard::new(), &mut self.backing) };
    }
}

/// A builder for elfmalloc allocators.
pub struct ElfBuilder {
    cutoff_factor: f64,
    page_size: Option<usize>,
    target_overhead: usize,
    eager_decommit_threshold: usize,
    max_objects: usize,
    elfmalloc_builder: ElfMallocBuilder,
}

impl Default for ElfBuilder {
    fn default() -> ElfBuilder {
        ElfBuilder {
            cutoff_factor: 0.6,
            page_size: None,
            target_overhead: 1 << 20,
            eager_decommit_threshold: 128 << 10,
            max_objects: 1 << 30,
            elfmalloc_builder: ElfMallocBuilder::default(),
        }
    }
}

impl ElfBuilder {
    pub fn cutoff_factor(mut self, cutoff_factor: f64) -> ElfBuilder {
        self.cutoff_factor = cutoff_factor;
        self
    }

    pub fn page_size(mut self, page_size: usize) -> ElfBuilder {
        self.page_size = Some(page_size);
        self.elfmalloc_builder.page_size = page_size;
        self
    }

    pub fn target_overhead(mut self, target_overhead: usize) -> ElfBuilder {
        self.target_overhead = target_overhead;
        self
    }

    pub fn eager_decommit_threshold(mut self, eager_decommit_threshold: usize) -> ElfBuilder {
        self.eager_decommit_threshold = eager_decommit_threshold;
        self
    }

    pub fn max_objects(mut self, max_objects: usize) -> ElfBuilder {
        assert!(max_objects > 0);
        self.max_objects = max_objects;
        self
    }

    pub fn target_pa_size(mut self, target_pa_size: usize) -> ElfBuilder {
        self.elfmalloc_builder.target_pa_size = target_pa_size;
        self
    }
    pub fn reuse_threshold(mut self, reuse_threshold: f64) -> ElfBuilder {
        self.elfmalloc_builder.reuse_threshold = reuse_threshold;
        self
    }
    pub fn max_object_size(mut self, max_object_size: usize) -> ElfBuilder {
        self.elfmalloc_builder.max_object_size = max_object_size;
        self
    }
    pub fn small_pipe_size(mut self, small_pipe_size: usize) -> ElfBuilder {
        assert!(small_pipe_size > 0);
        self.elfmalloc_builder.small_pipe_size = small_pipe_size;
        self
    }
    pub fn large_pipe_size(mut self, large_pipe_size: usize) -> ElfBuilder {
        assert!(large_pipe_size > 0);
        self.elfmalloc_builder.large_pipe_size = large_pipe_size;
        self
    }

    pub fn build_untyped_magazine(&self, layout: Layout) -> ElfUntypedObjectAlloc<MagazineCache> {
        let pages = self.mmap_page_cache(&layout);
        ElfUntypedObjectAlloc {
            inner: self.build_untyped_inner(pages, layout).alloc_expect("out of memory"),
        }
    }

    pub fn build_untyped_local(&self, layout: Layout) -> ElfUntypedObjectAlloc<LocalCache> {
        let pages = self.mmap_page_cache(&layout);
        ElfUntypedObjectAlloc {
            inner: self.build_untyped_inner(pages, layout).alloc_expect("out of memory"),
        }
    }

    pub(crate) fn build_untyped_inner<F, B>(&self, mut backing: B, layout: Layout) -> Option<ElfUntypedObjectAllocInner<F, B>>
    where
        F: Frontend<B>,
        B: GCAlloc,
    {
        self.validate(&backing, &layout);

        let metadata = Arc::new(compute_metadata(
                        layout.size(),
                        backing.layout().size(),
                        0, // local_index
                        0.6, // cutoff_factor
                        backing.layout().size(),
                        AllocType::SmallSlag, // not used; dummy value
                    ));
        let cfg = SlagConfig::new(NonNull::from(metadata.as_ref()), self.eager_decommit_threshold);
        let frontend = F::build(&cfg, &LazyGuard::new(), &mut backing)?;
        Some(ElfUntypedObjectAllocInner { backing, frontend, layout, metadata })
    }

    pub fn build_elfmalloc(&self) -> ElfMalloc {
        self.elfmalloc_builder.build()
    }

    /// Construct a `PageCache<MapAlloc>` from the present configuration and
    /// the given `Layout`.
    fn mmap_page_cache(&self, layout: &Layout) -> PageCache<MapAlloc> {
        let page_size = self.object_page_size(&layout);
        let alloc = MapAllocBuilder::default()
            .obj_size(page_size)
            .obj_align(page_size)
            .commit(cfg!(windows))
            .build();
        PageCache::new(alloc, page_size)
    }

    /// Calculate the page size used for an object allocator.
    fn object_page_size(&self, layout: &Layout) -> usize {
        if let Some(page_size) = self.page_size {
            page_size
        } else {
            cmp::max(32 << 10, layout.size() * 4)
        }
    }

    fn validate<B: GCAlloc>(&self, backing: &B, layout: &Layout) {
        assert!(layout.size() > 0);
        // TODO: Assert that we can fit at least one object in a slag of the
        // configured size. This check is insufficient because there's slag
        // header overhead.
        assert!(
            backing.layout().size() > layout.size(),
            "backing pages not larger than objects: {} <= {}",
            backing.layout().size(),
            layout.size(),
        );
    }
}

#[cfg(test)]
mod tests {
    extern crate env_logger;
    
    use std::collections::HashSet;
    use std::mem;
    use std::ptr::write_volatile;
    use std::thread;

    use super::*;

    #[test]
    fn obj_alloc_basic() {
        let _ = env_logger::init();
        let mut oa = ElfBuilder::default()
            .page_size(4096)
            .build_untyped_magazine(Layout::new::<usize>());
        unsafe {
            let item = oa.alloc().alloc_unwrap();
            write_volatile(item.as_ptr(), 10);
            oa.dealloc(item);
        }
    }

    #[test]
    fn obj_alloc_many_pages_single_threaded_usize() {
        obj_alloc_many_pages_single_threaded(Layout::new::<usize>());
    }

    #[test]
    fn obj_alloc_many_pages_single_threaded_u24() {
        obj_alloc_many_pages_single_threaded(Layout::new::<[u8; 24]>());
    }

    #[test]
    fn obj_alloc_many_pages_single_threaded_u32() {
        obj_alloc_many_pages_single_threaded(Layout::new::<[u8; 32]>());
    }

    fn obj_alloc_many_pages_single_threaded(layout: Layout) {
        let _ = env_logger::init();
        const N_ITEMS: usize = 4096 * 20;
        let mut oa = ElfBuilder::default().page_size(4096).build_untyped_magazine(layout.clone());
        alloc_assert!(layout.size() >= mem::size_of::<usize>());
        // stay in a local cache
        for _ in 0..N_ITEMS {
            unsafe {
                let item = oa.alloc().alloc_unwrap();
                write_volatile(item.cast().as_ptr(), 10);
                oa.dealloc(item);
            }
        }

        let mut v = Vec::with_capacity(N_ITEMS);
        let mut h = HashSet::new();
        for i in 0..N_ITEMS {
            unsafe {
                let item = oa.alloc().alloc_unwrap();
                write_volatile(item.cast().as_ptr(), i + 1);
                v.push(item);
                let item_num = item.as_ptr() as usize;
                alloc_assert!(!h.contains(&item_num));
                h.insert(item_num);
            }
        }

        for i in v {
            unsafe {
                oa.dealloc(i);
            }
        }
    }

    #[test]
    fn obj_alloc_many_pages_many_threads_usize() {
        let layout = Layout::new::<usize>();
        let builder = ElfBuilder::default().page_size(4096);
        obj_alloc_many_pages_many_threads(builder.build_untyped_magazine(layout.clone()));
        obj_alloc_many_pages_many_threads(builder.build_untyped_local(layout.clone()));
    }

    #[test]
    fn obj_alloc_many_pages_many_threads_u24() {
        let layout = Layout::new::<[u8; 24]>();
        let builder = ElfBuilder::default().page_size(4096);
        obj_alloc_many_pages_many_threads(builder.build_untyped_magazine(layout.clone()));
        obj_alloc_many_pages_many_threads(builder.build_untyped_local(layout.clone()));
    }

    #[test]
    fn obj_alloc_many_pages_many_threads_u32() {
        let layout = Layout::new::<[u8; 32]>();
        let builder = ElfBuilder::default().page_size(4096);
        obj_alloc_many_pages_many_threads(builder.build_untyped_magazine(layout.clone()));
        obj_alloc_many_pages_many_threads(builder.build_untyped_local(layout.clone()));
    }

    fn obj_alloc_many_pages_many_threads<F>(alloc: ElfUntypedObjectAlloc<F>)
    where
        F: Frontend<PageCache<MapAlloc>> + 'static,
    {
        let _ = env_logger::init();
        use std::mem;
        const N_ITEMS: usize = 4096 * 4;
        const N_THREADS: usize = 40;
        // stay in a local cache
        alloc_assert!(alloc.layout().size() >= mem::size_of::<usize>());
        let mut threads = Vec::new();
        for _ in 0..N_THREADS {
            let mut my_alloc = alloc.clone();
            threads.push(thread::spawn(move || {
                for _ in 0..N_ITEMS {
                    unsafe {
                        let item = my_alloc.alloc().alloc_unwrap();
                        write_volatile(item.cast().as_ptr(), 10);
                        my_alloc.dealloc(item);
                    }
                }

                let mut v = Vec::with_capacity(N_ITEMS);
                let mut h = HashSet::new();
                for i in 0..N_ITEMS {
                    unsafe {
                        let item = my_alloc.alloc().alloc_unwrap();
                        write_volatile(item.cast().as_ptr(), i);
                        v.push(item);
                        let item_num = item.as_ptr() as usize;
                        alloc_assert!(!h.contains(&item_num));
                        h.insert(item_num);
                    }
                }

                for i in v {
                    unsafe {
                        my_alloc.dealloc(i);
                    }
                }
            }));
        }
        for t in threads {
            t.join().alloc_expect("threads should exit successfully");
        }
    }
}
