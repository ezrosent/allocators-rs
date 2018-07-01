// Copyright 2017-2018 the authors. See the 'Copyright and license' section of the
// README.md file at the top-level directory of this repository.
//
// Licensed under the Apache License, Version 2.0 (the LICENSE-APACHE file) or
// the MIT license (the LICENSE-MIT file) at your option. This file may not be
// copied, modified, or distributed except according to those terms.

//! Implement various `Alloc`-specific traits.
//!
//! The reason why we do not do this directly is that Rust allocators have a different signature
//! than the C/C++ `malloc` ecosystem. In particular, deallocations pass object size and alignment
//! at the call site.
//!
//! This module also implements additional traits from the `malloc-bind` crate.

#[cfg(feature = "c-api")]
extern crate malloc_bind;
#[cfg(feature = "c-api")]
extern crate libc;

#[cfg(feature = "c-api")]
use std::intrinsics::unlikely;
use std::mem;
use std::ptr::NonNull;

use alloc::alloc::{Alloc, AllocErr, Layout};
#[cfg(feature = "c-api")]
use alloc_fmt::AllocUnwrap;
#[cfg(feature = "c-api")]
use self::malloc_bind::{LayoutFinder, Malloc, MIN_ALIGN};

use general::global;


#[cfg(feature = "c-api")]
use self::libc::{size_t, c_void};

/// A zero-sized type used for implementing `Alloc` and `LayoutFinder` for the global instance of
/// elfmalloc.
pub struct ElfMallocGlobal;

unsafe impl<'a> Alloc for &'a ElfMallocGlobal {
    unsafe fn alloc(&mut self, l: Layout) -> Result<NonNull<u8>, AllocErr> {
        // All objects are only guaranteed to be word-aligned except for powers of two. Powers of
        // two up to 1MiB are aligned to their size. Past that size, only page-alignment is
        // guaranteed.
        let ll = l.clone();
        if l.size().is_power_of_two() || l.align() <= mem::size_of::<usize>() {
            global::alloc(l.size())
        } else {
            global::alloc(l.size().next_power_of_two())
        }.ok_or(AllocErr)
    }

    unsafe fn dealloc(&mut self, p: NonNull<u8>, _l: Layout) {
        global::dealloc(p);
    }

    unsafe fn realloc(&mut self, p: NonNull<u8>, _l1: Layout, new_size: usize) -> Result<NonNull<u8>, AllocErr> {
        let (size, align) = unimplemented!();
        global::aligned_realloc(p, size, align)
            .ok_or(AllocErr)
    }
}

#[cfg(feature = "c-api")]
unsafe impl Malloc for ElfMallocGlobal {
    unsafe fn c_malloc(&self, size: size_t) -> *mut c_void {
        let p = global::alloc(size as usize)
                    .map(NonNull::as_ptr)
                    .unwrap_or(ptr::null_mut()) as *mut c_void;
        alloc_debug_assert_eq!((p as usize) % MIN_ALIGN,
                         0,
                         "object does not have the required alignment of {}: {:?}",
                         MIN_ALIGN,
                         p);
        p
    }
    unsafe fn c_free(&self, p: *mut c_void) {
        alloc_debug_assert_eq!((p as usize) % MIN_ALIGN,
                         0,
                         "object does not have the required alignment of {}: {:?}",
                         MIN_ALIGN,
                         p);
        if unlikely(p.is_null()) {
            return;
        }
        global::dealloc(p as *mut u8)
    }
    unsafe fn c_realloc(&self, p: *mut c_void, new_size: size_t) -> *mut c_void {
        alloc_debug_assert_eq!((p as usize) % MIN_ALIGN,
                         0,
                         "object does not have the required alignment of {}: {:?}",
                         MIN_ALIGN,
                         p);
        if p.is_null() {
            return self.c_malloc(new_size);
        }
        if cfg!(any(target_os = "linux", target_os = "windows")) && new_size == 0 {
            self.c_free(item);
            return ptr::null_mut();
        }
        let p = global::realloc(p as *mut u8, new_size as usize)
            .map(NonNull::as_ptr)
            .unwrap_or(ptr::null_mut()) as *mut c_void;
        alloc_debug_assert_eq!((p as usize) % MIN_ALIGN,
                         0,
                         "object does not have the required alignment of {}: {:?}",
                         MIN_ALIGN,
                         p);
        p
    }
}

#[cfg(feature = "c-api")]
unsafe impl LayoutFinder for ElfMallocGlobal {
    unsafe fn get_layout(&self, p: *mut u8) -> Layout {
        // Note that per the current implementation of malloc-bind, we could just return a dummy
        // value, e.g.  Layout::from_size_align(8, 8).unwrap()
        // But, seeing as that (and `Alloc`'s default impl's internals) may change, we are going to
        // err on the side of caution for the time being
        let (size, align) = global::get_layout(p);
        Layout::from_size_align(size, align).alloc_unwrap()
    }
}
