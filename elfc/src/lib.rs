// Copyright 2017 the authors. See the 'Copyright and license' section of the
// README.md file at the top-level directory of this repository.
//
// Licensed under the Apache License, Version 2.0 (the LICENSE file). This file
// may not be copied, modified, or distributed except according to those terms.

extern crate elfmalloc;
extern crate errno;
extern crate libc;
extern crate sysconf;
extern crate env_logger;
use errno::{Errno, set_errno};
use libc::{c_void, size_t, c_int};
use elfmalloc::general::global;
use std::ptr;
use sysconf::page::pagesize;

#[no_mangle]
pub extern "C" fn malloc(bytes: size_t) -> *mut c_void {
    unsafe { global::alloc(bytes) as *mut c_void }
}

#[no_mangle]
pub extern "C" fn realloc(ptr: *mut c_void, bytes: size_t) -> *mut c_void {
    unsafe { global::realloc(ptr as *mut u8, bytes) as *mut c_void }
}

#[no_mangle]
pub extern "C" fn calloc(nmemb: size_t, size: size_t) -> *mut c_void {
    #[cfg(feature = "logging")]
    {
        let _ = env_logger::init();
    }
    // TODO check for overflow, etc.
    let bytes = nmemb * size;
    let res = malloc(bytes);
    unsafe {
        ptr::write_bytes(res, 0, bytes);
    }
    res
}

#[no_mangle]
pub extern "C" fn free(ptr: *mut c_void) {
    if ptr.is_null() {
        return;
    }
    unsafe { global::free(ptr as *mut u8) }
}

#[no_mangle]
pub extern "C" fn cfree(ptr: *mut c_void) {
    free(ptr)
}

#[no_mangle]
pub extern "C" fn memalign(_alignment: size_t, size: size_t) -> *mut c_void {
    // we align all power-of-two sizes to their size
    malloc(size.next_power_of_two())
}

#[no_mangle]
pub extern "C" fn aligned_alloc(alignment: size_t, size: size_t) -> *mut c_void {
    if size % alignment != 0 {
        set_errno(Errno(libc::EINVAL));
        return ptr::null_mut();
    }
    memalign(alignment, size)
}

#[cfg_attr(feature="cargo-clippy", allow(not_unsafe_ptr_arg_deref))]
#[no_mangle]
pub extern "C" fn posix_memalign(p: *mut *mut c_void, align: size_t, size: size_t) -> c_int {
    unsafe {
        (*p) = memalign(align, size);
    }
    0
}

#[no_mangle]
pub extern "C" fn valloc(bytes: size_t) -> *mut c_void {
    use std::cmp;
    memalign(pagesize(), cmp::max(pagesize(), bytes))
}

#[no_mangle]
pub extern "C" fn pvalloc(bytes: size_t) -> *mut c_void {
    valloc(bytes)
}

#[no_mangle]
pub extern "C" fn malloc_stats() {}

#[no_mangle]
pub extern "C" fn mallopt(_cmd: c_int, _value: c_int) -> c_int {
    0
}
