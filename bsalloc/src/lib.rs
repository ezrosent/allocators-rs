// Copyright 2017-2018 the authors. See the 'Copyright and license' section of
// the README.md file at the top-level directory of this repository.
//
// Licensed under the Apache License, Version 2.0 (the LICENSE-APACHE file) or
// the MIT license (the LICENSE-MIT file) at your option. This file may not be
// copied, modified, or distributed except according to those terms.

#![no_std]
#![feature(allocator_api)]

extern crate alloc;
#[macro_use]
extern crate alloc_fmt;
#[macro_use]
extern crate lazy_static;
extern crate mmap_alloc;

mod bsalloc;

use alloc::alloc::{Alloc, GlobalAlloc, Layout};
use core::{cmp, ptr};

use bsalloc::{BsAlloc, ALLOC};

#[cfg(debug_assertions)]
fn assert_nonoverlapping(r1: (usize, usize), r2: (usize, usize)) {
    let (_, b) = cmp::min(r1, r2);
    let (c, _) = cmp::max(r1, r2);
    alloc_assert!(c >= b);
}

#[cfg(not(test))]
#[global_allocator]
static GLOBAL: BsAlloc = BsAlloc;

unsafe impl GlobalAlloc for BsAlloc {
    unsafe fn alloc(&self, l: Layout) -> *mut u8 {
        (*ALLOC).alloc(l.size())
    }

    unsafe fn dealloc(&self, item: *mut u8, l: Layout) {
        (*ALLOC).free(item, l.size())
    }

    unsafe fn realloc(&self, ptr: *mut u8, layout: Layout, new_size: usize) -> *mut u8 {
        let old_size = layout.size();
        // TODO(joshlf): To technically implement realloc correctly, we need to
        // preserve the alignment of the old allocation. To do this, we need to
        // round up new_size to a multiple of that alignment.
        let new_memory = self.alloc(Layout::from_size_align(new_size, 1).unwrap());
        if new_memory.is_null() {
            return new_memory;
        }
        #[cfg(debug_assertions)]
        assert_nonoverlapping(
            (ptr as usize, ptr as usize + old_size),
            (new_memory as usize, new_memory as usize + new_size),
        );
        ptr::copy_nonoverlapping(ptr, new_memory, cmp::min(old_size, new_size));
        self.dealloc(ptr, layout);
        new_memory
    }
}
