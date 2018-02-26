// Copyright 2017-2018 the authors. See the 'Copyright and license' section of the
// README.md file at the top-level directory of this repository.
//
// Licensed under the Apache License, Version 2.0 (the LICENSE-APACHE file) or
// the MIT license (the LICENSE-MIT file) at your option. This file may not be
// copied, modified, or distributed except according to those terms.

//! Low-level data-structures for getting more memory from the system.
use std::sync::Arc;
use std::sync::atomic::{AtomicUsize, AtomicPtr, Ordering};
use std::mem;
use super::utils::{likely, mmap};

/// A generator of chunks of memory providing an `sbrk`-like interface.
pub trait MemorySource
where
    Self: Clone,
{
    fn new(page_size: usize) -> Option<Self>;
    /// The smallest unit of memory that can be `carve`d.
    fn page_size(&self) -> usize;
    /// Return `npages` fresh pages from the `Creek`. Each of these pages is aligned to
    /// `page_size`.
    ///
    /// Currently, there is code (see the `Coalescer` in the `slag` module) that relies on fresh
    /// pages returned from `carve` to be filled with zeros.
    fn carve(&self, npages: usize) -> Option<*mut u8>;
}

/// A `MemorySource` that can tell which pointers lie in a region returned by `carve`.
pub trait MemoryBlock: MemorySource {
    /// Is `it` a pointer to somewhere in the block of memory.
    fn contains(&self, it: *mut u8) -> bool;
}


/// A `MemorySource` that just calls mmap. It still maintains that all pages returned by `carve`
/// are aligned to their size.
#[derive(Copy, Clone)]
pub struct MmapSource {
    page_size: usize,
}

unsafe impl Send for MmapSource {}

impl MemorySource for MmapSource {
    fn new(page_size: usize) -> Option<MmapSource> {
        Some(MmapSource { page_size: page_size.next_power_of_two() })
    }
    fn page_size(&self) -> usize {
        self.page_size
    }

    fn carve(&self, npages: usize) -> Option<*mut u8> {
        trace!("carve({:?})", npages);
        // faster mod for power-of-2 sizes.
        fn mod_size(x: usize, n: usize) -> usize {
            x & (n - 1)
        }

        // There is a faster path available when our local page size is less than or equal to the
        // system one.
        let system_page_size = mmap::page_size();
        if self.page_size <= system_page_size {
            return mmap::map(npages * self.page_size);
        }
        // We want to return pages aligned to our page size, which is larger than the
        // system page size. As a result, we want to allocate an extra page to guarantee a slice of
        // the memory that is aligned to the larger page size.
        let target_size = npages * self.page_size;
        let req_size = target_size + self.page_size;
        mmap::map(req_size).and_then(|mem| {
            let mem_num = mem as usize;

            alloc_debug_assert_eq!(mod_size(mem_num, system_page_size), 0);

            // region at the end that is not needed.
            let rem1 = mod_size(mem_num, self.page_size);
            // region at the beginning that is not needed.
            let rem2 = self.page_size - rem1;
            unsafe {
                let res = mem.offset(rem2 as isize);
                alloc_debug_assert_eq!(mod_size(res as usize, self.page_size), 0);
                if rem1 > 0 {
                    mmap::unmap(res.offset(target_size as isize), rem1);
                }
                if rem2 > 0 {
                    mmap::unmap(mem, rem2);
                }
                Some(res)
            }
        })
    }
}

/// Base address and size of a memory map.
///
/// This could also just be a `*mut [u8]`, but having two fields is more explicit. We need a new
/// type because the `Drop` implementation calls `unmap`.
#[derive(Debug)]
struct MapAddr(*mut u8, usize);

impl Drop for MapAddr {
    fn drop(&mut self) {
        unsafe {
            mmap::unmap(self.0, self.1);
        }
    }
}

/// A large, contiguous, memory-mapped region of memory.
///
/// A `Creek` can be seen as a very basic memory allocator that can hand back multiples of its
/// `page_size`. While it does not implement `free`, the programmer can still call `uncommit` or
/// `unmap` on pages that are returned from a `Creek`. In this module, the `Creek` is used to
/// manage the (thread-safe) introduction of fresh (clean) pages into the rest of the allocator to
/// be used.
///
/// `Creek`s are initialized with a maximum size and never grow beyond that size. They are made
/// with a particular idiom in mind in which a larger-than-physical-memory mapping is requested for
/// the `Creek`, only a fraction of which is ever used by the running program (hence, ever backed
/// by physical page frames).
#[derive(Debug)]
pub struct Creek {
    page_size: usize,
    /// We use a `clone`-based interface in order to facilitate passing across threads and
    /// leveraging `Arc` to call `unmap`.
    map_info: Arc<MapAddr>,
    base: *mut u8,
    bump: AtomicPtr<AtomicUsize>,
}

unsafe impl Send for MapAddr {}
unsafe impl Sync for MapAddr {}
unsafe impl Send for Creek {}
unsafe impl Sync for Creek {}

macro_rules! check_bump {
    ($slf:expr) => {
        #[cfg(debug_assertions)]
        {
            let bump = $slf.bump.load(Ordering::Relaxed);
            alloc_debug_assert!(!bump.is_null());
        }
    };
}

impl MemorySource for Creek {
    #[inline]
    fn page_size(&self) -> usize {
        check_bump!(self);
        self.page_size
    }

    fn carve(&self, npages: usize) -> Option<*mut u8> {
        check_bump!(self);
        unsafe {
            let new_bump = self.bump
                .load(Ordering::Relaxed)
                .as_ref()
                .unwrap()
                .fetch_add(npages, Ordering::Relaxed);
            if likely((new_bump + npages) * self.page_size < self.map_info.1) {
                Some(self.base.offset((new_bump * self.page_size) as isize))
            } else {
                None
            }
        }
    }


    /// Create a new `Creek` with pages of size `page_size` total heap size of `heap_size`,
    /// optionally backed by huge pages.
    ///
    /// Page size and heap size should be powers of two. The allocator may want to reserve some
    /// pages for itself (or for alignment reasons), as a result it is a good idea to have
    /// heap_size be much larger than page_size.
    fn new(page_size: usize) -> Option<Self> {
        // lots of stuff breaks if this isn't true
        alloc_assert!(page_size.is_power_of_two());
        alloc_assert!(page_size > mem::size_of::<usize>());
        // first, let's grab some memory;
        let (orig_base, heap_size) = {
            let mut heap_size: usize = 2 << 40;
            loop {
                if let Some(heap) = self::mmap::map(heap_size) {
                    break (heap, heap_size);
                } else if heap_size <= (1 << 30) {
                    return None;
                }
                heap_size /= 2;
            }
        };
        info!("created heap of size {}", heap_size);
        let orig_addr = orig_base as usize;
        let (slush_addr, real_addr) = {
            // allocate some `slush` space at the beginning of the creek. This gives us space to
            // store the `bump` pointer. In the future, we may store more things in this slush
            // space as well.
            //
            // In addition, we must ensure that pages are aligned to their size.
            let base = if orig_addr == 0 {
                // this is a real possibility if we are calling `mmap` directly.
                // However, `MmapAlloc` currently handles `mmap` returning null, so this is
                // technically a redundant check.
                orig_addr + page_size
            } else if orig_addr % page_size != 0 {
                let rem = orig_addr % page_size;
                orig_addr + (page_size - rem)
            } else {
                orig_addr
            };
            (base as *mut u8, (base + page_size) as *mut u8)
        };
        Some(Creek {
            page_size: page_size,
            map_info: Arc::new(MapAddr(orig_base, heap_size)),
            base: real_addr,
            bump: AtomicPtr::new(slush_addr as *mut AtomicUsize),
        })
    }
}

impl MemoryBlock for Creek {
    fn contains(&self, it: *mut u8) -> bool {
        check_bump!(self);
        let it_num = it as usize;
        let base_num = self.base as usize;
        it_num >= base_num && it_num < base_num + self.map_info.1
    }
}

impl Clone for Creek {
    fn clone(&self) -> Self {
        let bump = self.bump.load(Ordering::Relaxed);
        alloc_debug_assert!(!bump.is_null());
        Creek {
            page_size: self.page_size,
            map_info: self.map_info.clone(),
            base: self.base,
            bump: AtomicPtr::new(bump),
        }
    }
}
