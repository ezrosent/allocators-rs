// Copyright 2017-2018 the authors. See the 'Copyright and license' section of the
// README.md file at the top-level directory of this repository.
//
// Licensed under the Apache License, Version 2.0 (the LICENSE-APACHE file) or
// the MIT license (the LICENSE-MIT file) at your option. This file may not be
// copied, modified, or distributed except according to those terms.

use super::core::sync::atomic::{AtomicPtr, Ordering};
use super::core::mem;
use super::core::ptr;

use super::mmap_alloc::MapAlloc;
#[cfg(windows)]
use super::mmap_alloc::MapAllocBuilder;
use super::{Alloc, Layout, AllocErr};

#[derive(Copy, Clone)]
pub struct BsAlloc;

lazy_static! {
    pub static ref ALLOC: GlobalAllocator = GlobalAllocator::new();
}

pub struct GlobalAllocator {
    small_objs: large::Cache,
    large_objs: small::Cache,
    ma: MapAlloc,
}

impl GlobalAllocator {
    fn new() -> Self {
        // On Windows, memory must be explicitly committed - it will not simply be committed on
        // first use like on Linux and Mac.
        #[cfg(windows)]
        let ma = MapAllocBuilder::default().commit(true).build();
        #[cfg(not(windows))]
        let ma = MapAlloc::default();

        GlobalAllocator {
            small_objs: large::Cache::new(ma.usable_size(&Layout::from_size_align(1<<10, 1).unwrap()).1),
            large_objs: small::Cache::new(ma.usable_size(&Layout::from_size_align(18<<10, 1).unwrap()).1),
            ma,
        }
    }

    pub unsafe fn alloc(&self, size: usize) -> Result<*mut u8, AllocErr> {
        let mut ma = &self.ma;
        if size < self.small_objs.object_size {
            self.small_objs.alloc(ma)
        } else if size < self.large_objs.object_size {
            self.large_objs.alloc(ma)
        } else {
            ma.alloc(Layout::from_size_align(size, 1).unwrap())
        }
    }

    pub unsafe fn free(&self, item: *mut u8, size: usize) {
        let mut ma = &self.ma;
        if size < self.small_objs.object_size {
            self.small_objs.free(item, ma);
            return;
        }
        if size < self.large_objs.object_size {
            self.large_objs.free(item, ma);
            return;
        }
        ma.dealloc(item, Layout::from_size_align(size, 1).unwrap())
    }
}

fn rng() -> usize {
    const RAND_A: usize = 16_807;
    let seed_ptr: usize = 0;
    let seed = ((&seed_ptr) as *const _ as usize) / mem::align_of_val(&seed_ptr);
    seed * RAND_A
}


macro_rules! sized_cache {
    ($name:ident, $cache_size:expr) => {
        mod $name {
            use super::*;
            const CACHE_SIZE: usize = $cache_size;
            const PATIENCE: usize = CACHE_SIZE>>2;
            pub struct Cache {
                pub object_size: usize,
                layout: Layout,
                cache: [AtomicPtr<u8>; CACHE_SIZE],
            }

            impl Cache {
                pub fn new(obj_size: usize) -> Self {
                    let res = Cache {
                        object_size: obj_size,
                        layout: Layout::from_size_align(obj_size, 1).unwrap(),
                        cache: unsafe {
                            mem::transmute::<[usize; CACHE_SIZE], [AtomicPtr<u8>; CACHE_SIZE]>([0; CACHE_SIZE])
                        },
                    };
                    res
                }

                pub unsafe fn alloc(&self, mut ma: &MapAlloc) -> Result<*mut u8, AllocErr> {
                    let start = rng();
                    let mut cur_slot = start % CACHE_SIZE;
                    let stride = cur_slot * 2 + 1;
                    for _ in 0..PATIENCE {
                        let p = self.cache.get_unchecked(cur_slot).swap(ptr::null_mut(), Ordering::Relaxed);
                        if p.is_null() {
                            cur_slot += stride;
                            cur_slot %= CACHE_SIZE;
                            continue;
                        }
                        return Ok(p);
                    }
                    ma.alloc(self.layout.clone())
                }

                pub unsafe fn free(&self, item: *mut u8, mut ma: &MapAlloc) {
                    let mut cur_ptr = item;
                    let start = rng();
                    let mut cur_slot = start % CACHE_SIZE;
                    let stride = cur_slot * 2 + 1;
                    for _ in 0..PATIENCE {
                        cur_ptr = self.cache.get_unchecked(cur_slot).swap(cur_ptr, Ordering::Relaxed);
                        if cur_ptr.is_null() {
                            return;
                        }
                        cur_slot += stride;
                        cur_slot %= CACHE_SIZE;
                    }
                    ma.dealloc(cur_ptr, self.layout.clone());
                }
            }

        }
    };
}
sized_cache!(large, 8);
sized_cache!(small, 4);

#[cfg(test)]
mod tests {
    use super::*;
    use core::ptr::write_volatile;
    #[test]
    fn test_alloc_basic() {
        for size in 1..((1 << 18) + 1) {
            let layout = Layout::from_size_align(size * 8, 8).unwrap();
            unsafe {
                let item = (&BsAlloc).alloc(layout.clone()).unwrap();
                write_volatile(item, 10);
                (&BsAlloc).dealloc(item, layout);
            }
        }
    }
}
