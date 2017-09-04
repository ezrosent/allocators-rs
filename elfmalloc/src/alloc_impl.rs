// Copyright 2017 the authors. See the 'Copyright and license' section of the
// README.md file at the top-level directory of this repository.
//
// Licensed under the Apache License, Version 2.0 (the LICENSE file). This file
// may not be copied, modified, or distributed except according to those terms.

//! Implement various `Alloc`-specific traits.
//!
//! The reason why we do not do this directly is that Rust allocators have a different signature
//! than the C/C++ `malloc` ecosystem. In particular, deallocations pass object size and alignment
//! at the call site.
//!
//! This module also implements additional traits from the `malloc-bind` crate.

extern crate alloc;
extern crate malloc_bind;
use self::alloc::allocator::{Alloc, AllocErr, Layout};
use self::malloc_bind::LayoutFinder;
use super::general::global;
use std::mem;

/// A zero-sized type used for implementing `Alloc` and `LayoutFinder` for the global instance of
/// elfmalloc.
pub struct ElfMallocGlobal;

unsafe impl<'a> Alloc for &'a ElfMallocGlobal {
    unsafe fn alloc(&mut self, l: Layout) -> Result<*mut u8, AllocErr> {
        // All objects are only guaranteed to be word-aligned except for powers of two. Powers of
        // two up to 1MiB are aligned to their size. Past that size, only page-alignment is
        // guaranteed.
        if l.size().is_power_of_two() || l.align() <= mem::size_of::<usize>() {
            Ok(global::alloc(l.size()))
        } else {
            Ok(global::alloc(l.size().next_power_of_two()))
        }
    }

    unsafe fn dealloc(&mut self, p: *mut u8, _l: Layout) {
        global::free(p);
    }

    unsafe fn realloc(&mut self, p: *mut u8, _l1: Layout, l2: Layout) -> Result<*mut u8, AllocErr> {
        Ok(global::aligned_realloc(p, l2.size(), l2.align()))
    }
}

impl<'a> LayoutFinder for &'a ElfMallocGlobal {
    fn get_layout(&self, p: *mut u8) -> Layout {
        // Note that per the current implementation of malloc-bind, we could just return a dummy
        // value, e.g.  Layout::from_size_align(8, 8).unwrap()
        // But, seeing as that (and `Alloc`'s default impl's internals) may change, we are going to
        // err on the side of caution for the time being
        let (size, align) = unsafe { global::get_layout(p) };
        Layout::from_size_align(size, align).unwrap()
    }
}
