// Copyright 2017-2018 the authors. See the 'Copyright and license' section of the
// README.md file at the top-level directory of this repository.
//
// Licensed under the Apache License, Version 2.0 (the LICENSE-APACHE file) or
// the MIT license (the LICENSE-MIT file) at your option. This file may not be
// copied, modified, or distributed except according to those terms.

#![no_std]
#![feature(allocator_api)]
#![feature(alloc)]
#![feature(global_allocator)]
extern crate alloc;
#[cfg(debug_assertions)]
#[macro_use]
extern crate alloc_fmt;
#[macro_use]
extern crate lazy_static;
extern crate mmap_alloc;
mod bsalloc;
use bsalloc::{BsAlloc, ALLOC};
use self::alloc::allocator::{Alloc, AllocErr, Layout};
use core::cmp;
use core::ptr;

#[cfg(debug_assertions)]
fn assert_nonoverlapping(r1: (usize, usize), r2: (usize, usize)) {
    let (_, b) = cmp::min(r1, r2);
    let (c, _) = cmp::max(r1, r2);
    alloc_assert!(c >= b);
}

#[global_allocator]
static GLOBAL: BsAlloc = BsAlloc;

unsafe impl<'a> Alloc for &'a BsAlloc {
    unsafe fn alloc(&mut self, l: Layout) -> Result<*mut u8, AllocErr> {
        (*ALLOC).alloc(l.size())
    }

    unsafe fn dealloc(&mut self, item: *mut u8, l: Layout) {
        (*ALLOC).free(item, l.size())
    }

    unsafe fn realloc(&mut self,
                      ptr: *mut u8,
                      old_l: Layout,
                      new_l: Layout)
                      -> Result<*mut u8, AllocErr> {
        let old_size = old_l.size();
        let new_size = new_l.size();
        let new_memory = self.alloc(new_l).expect("alloc failed");
        #[cfg(debug_assertions)]
        {
            assert_nonoverlapping((ptr as usize, ptr as usize + old_size),
                                  (new_memory as usize, new_memory as usize + new_size));
        }
        ptr::copy_nonoverlapping(ptr, new_memory, cmp::min(old_size, new_size));
        self.dealloc(ptr, old_l);
        Ok(new_memory)
    }
}
