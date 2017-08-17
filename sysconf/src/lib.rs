//! Query runtime configuration information.
//!
//! This crate provides the ability to query for various configuration information about the
//! runtime platform such as memory page size. On POSIX systems, it makes heavy use of the
//! [sysconf] API.
//!
//! [sysconf]: http://man7.org/linux/man-pages/man3/sysconf.3.html

// NOTE: According to Wikipedia, "Originally, the name "POSIX" referred to IEEE Std 1003.1-1988,
// released in 1988." This crate assumes that any behavior required by POSIX 1003.1 will be
// properly implemented on any POSIX system. Running on a POSIX system which does not adhere to
// these requirements will cause either an assertion failure/panic or undefined behavior.

#![cfg_attr(not(test), no_std)]

extern crate libc;
#[cfg(windows)]
extern crate kernel32;
#[cfg(windows)]
extern crate winapi;
#[cfg(any(target_os = "linux", target_os = "macos"))]
extern crate errno;
#[cfg(test)] // In tests, we disable no_std, so core isn't automatically included
extern crate core;
#[macro_use]
extern crate lazy_static;

pub mod hugepage;

#[cfg(any(target_os = "linux", target_os = "macos"))]
use errno::errno;

/// Get the system's page size.
#[cfg(any(unix, windows))]
#[cfg_attr(feature = "cargo-clippy", allow(inline_always))]
#[inline(always)]
pub fn pagesize() -> usize {
    *PAGESIZE
}

#[cfg(any(unix, windows))]
lazy_static!{ static ref PAGESIZE: usize = priv_pagesize(); }

#[cfg(unix)]
fn priv_pagesize() -> usize {
    // sysconf(_SC_PAGESIZE) is required by POSIX 1003.1: http://www.unix.com/man-page/posix/3p/sysconf/
    use self::libc::{sysconf, _SC_PAGESIZE, EINVAL};
    let pagesize = unsafe { sysconf(_SC_PAGESIZE) };
    if pagesize < 1 {
        assert_eq!(errno().0, EINVAL);
        panic!("sysconf(_SC_PAGESIZE) returned EINVAL, but _SC_PAGESIZE is required by POSIX 1003.1");
    }
    pagesize as usize
}

#[cfg(windows)]
fn priv_pagesize() -> usize {
    use self::kernel32::GetSystemInfo;
    use self::winapi::SYSTEM_INFO;
    use core::mem::uninitialized;
    unsafe {
        let mut info = uninitialized::<SYSTEM_INFO>();
        GetSystemInfo(&mut info);
        info.dwPageSize as usize
    }
}
