// Copyright 2017-2018 the authors. See the 'Copyright and license' section of the
// README.md file at the top-level directory of this repository.
//
// Licensed under the Apache License, Version 2.0 (the LICENSE-APACHE file) or
// the MIT license (the LICENSE-MIT file) at your option. This file may not be
// copied, modified, or distributed except according to those terms.

//! Allocator-safe formatting and assertion macros.
//!
//! [`alloc-fmt`] provides formatting and assertion macros similar to the standard library's
//! [`println`], [`eprintln`], [`panic`], [`assert`], [`debug_assert`], etc which are safe for use in a
//! global allocator. The standard library's formatting and assertion macros can allocate, meaning
//! that if they are used in the implementation of a global allocator, it can cause infinite
//! recursion. The macros in this crate avoid this problem by either not allocating (in the case of
//! formatting macros) or detecting recursion (in the case of panic and assertion macros).
//!
//! [`alloc-fmt`]: index.html
//! [`println`]: https://doc.rust-lang.org/std/macro.println.html
//! [`eprintln`]: https://doc.rust-lang.org/std/macro.eprint.html
//! [`panic`]: https://doc.rust-lang.org/std/macro.panic.html 
//! [`assert`]: https://doc.rust-lang.org/std/macro.assert.html
//! [`debug_assert`]: https://doc.rust-lang.org/std/macro.debug_assert.html
//!
//! # Usage and Behavior
//! The macros in this crate are named `alloc_xxx`, where `xxx` is the name of the equivalent
//! standard library macro (e.g., [`alloc_println`], [`alloc_debug_assert`], etc).
//!
//! The behavior of the formatting macros is identical to the behavior of their standard library
//! counterparts. The behavior of the panic and assertion macros is somewhat different. When an
//! assertion fails or an explicit panic is invoked, a message is first unconditionally printed to
//! stderr (in case further processing causes a crash). A stack trace is then printed, and the
//! process aborts. If recursion is detected during the printing of the stack trace, the process
//! immediately aborts. Recusion can happen if, for example, the code that computes the stack trace
//! allocates, triggering further assertion failures or panics. This check is conservative - it may
//! sometimes detect recursion when there is none.
//!
//! Unlike the standard library assertion and panic macros, the stack is not unwound, and once an
//! assertion failure or panic triggers, it cannot be caught or aborted.
//!
//! [`alloc_println`]: macro.alloc_println.html
//! [`alloc_debug_assert`]: macro.alloc_debug_assert.html

#![no_std]
#![feature(core_intrinsics)]

#[cfg(feature = "print-backtrace")]
extern crate backtrace;
extern crate libc;
extern crate spin;

use core::fmt::{Arguments, Result as FmtResult, Write};
use core::sync::atomic::{AtomicBool};
use core::sync::atomic::Ordering::SeqCst;

// Import items that macros need to reference. They will reference them as $crate::foo instead of
// core::foo since core isn't guaranteed to be imported in the user's scope.
#[doc(hidden)]
pub use core::mem::drop;
#[doc(hidden)]
pub use core::fmt::write;

#[doc(hidden)]
pub static STDERR_MTX: spin::Mutex<()> = spin::Mutex::new(());
#[doc(hidden)]
pub static STDOUT_MTX: spin::Mutex<()> = spin::Mutex::new(());

#[doc(hidden)]
pub static STDOUT: libc::c_int = 1;
#[doc(hidden)]
pub static STDERR: libc::c_int = 2;

#[doc(hidden)]
pub struct FDWriter(pub libc::c_int);

impl Write for FDWriter {
    #[inline]
    fn write_str(&mut self, s: &str) -> FmtResult {
        let mut buf = s.as_bytes();
        while !buf.is_empty() {
            unsafe {
                #[cfg(not(windows))]
                let written = libc::write(self.0, buf.as_ptr() as *const _, buf.len());
                #[cfg(windows)]
                let written =
                    libc::write(self.0, buf.as_ptr() as *const _, buf.len() as libc::c_uint);
                if written < 1 {
                    core::intrinsics::abort();
                }
                buf = &buf[written as usize..];
            }
        }
        Ok(())
    }
}

// We can't simply 'pub use core::intrinsics::abort' because its use requires
// feature(core_intrinsics), which the user would have to enable.
#[doc(hidden)]
pub unsafe fn abort() -> ! {
    core::intrinsics::abort();
}

#[doc(hidden)]
#[macro_export]
macro_rules! print_internal {
    ($file:expr, $mtx:expr, $fmt:expr) => {
        // in case we're called from inside an unsafe block
        #[allow(unused_unsafe)]
        unsafe {
            let guard = $mtx.lock();

            let mut fd = $crate::FDWriter($file);
            let _ = $crate::write(&mut fd, $fmt).map_err(|_| {
                $crate::abort();
            });

            // explicitly drop the guard so we're guaranteed it lives at least this long;
            // since we don't actually use the guard, the compiler could theoretically
            // drop it sooner.
            $crate::drop(guard);
        }
    }
}

#[macro_export]
macro_rules! alloc_print {
    ($($arg:tt)*) => (print_internal!($crate::STDOUT, $crate::STDOUT_MTX, format_args!($($arg)*)))
}

#[macro_export]
macro_rules! alloc_eprint {
    ($($arg:tt)*) => (print_internal!($crate::STDERR, $crate::STDERR_MTX, format_args!($($arg)*)))
}

#[macro_export]
macro_rules! alloc_println {
    () => (alloc_print!("\n"));
    ($fmt:expr) => (alloc_print!(concat!($fmt, "\n")));
    ($fmt:expr, $($arg:tt)*) => (alloc_print!(concat!($fmt, "\n"), $($arg)*));
}

#[macro_export]
macro_rules! alloc_eprintln {
    () => (alloc_eprint!("\n"));
    ($fmt:expr) => (alloc_eprint!(concat!($fmt, "\n")));
    ($fmt:expr, $($arg:tt)*) => (alloc_eprint!(concat!($fmt, "\n"), $($arg)*));
}

// Sometimes backtraces allocate. If that allocation causes an assertion failure, then we can get
// into an infinite recursion scenario. In order to detect this, we set this global bool to true,
// and load it before printing a backtrace. If we find that it is true, we immediately abort
// (essentially short-circuiting what would eventually happen if print_backtrace_and_abort
// successfully finished executing).
#[doc(hidden)]
pub static IS_PANICKING: AtomicBool = AtomicBool::new(false);

#[macro_export]
macro_rules! alloc_panic {
    () => (alloc_panic!("explicit panic"));
    ($msg:expr) => ({
        $crate::panic(&(format_args!($msg), file!(), line!(), column!()))
    });
    ($fmt:expr, $($arg:tt)*) => ({
        $crate::panic(&(format_args!($fmt, $($arg)*), file!(), line!(), column!()))
    })
}

#[doc(hidden)]
#[inline(never)]
#[cold]
pub fn panic(fmt_file_line_col: &(Arguments, &'static str, u32, u32)) -> ! {
    let (fmt, file, line, col) = *fmt_file_line_col;
    alloc_eprint!("thread panicked at '");
    print_internal!(STDERR, STDERR_MTX, fmt);
    alloc_eprintln!("', {}:{}:{}", file, line, col);
    unsafe {
        if IS_PANICKING.compare_and_swap(false, true, SeqCst) {
            // compare_and_swap returns the old value; true means somebody's already panicking.
            alloc_eprintln!("thread panicked while panicking");
            core::intrinsics::abort();
        }
        print_backtrace_and_abort()
    }
}

#[macro_export]
macro_rules! alloc_assert {
    ($pred:expr) => ({
        // Do this instead of alloc_assert!($pred, stringify!($pred)) in case $pred contains
        // characters that would be interpreted as formatting directives.
        alloc_assert!($pred, "{}", stringify!($pred));
    });
    ($pred:expr, $msg:expr) => ({
        if !($pred) {
            alloc_panic!("assertion failed: {}", $msg);
        }
    });
    ($pred:expr, $fmt:expr, $($arg:tt)*) => ({
        if !($pred) {
            alloc_panic!(concat!("assertion failed: ", $fmt), $($arg)*);
        }
    })
}

#[macro_export]
macro_rules! alloc_debug_assert {
    ($($arg:tt)*) => {
        if cfg!(debug_assertions) {
            alloc_assert!($($arg)*);
        }
    }
}

#[macro_export]
macro_rules! alloc_assert_eq {
    ($a:expr, $b:expr) => {
        {
            let a = $a;
            let b = $b;
            let s = stringify!($a == $b);
            alloc_assert!(a == b, "{} (evaluated to {:?} == {:?})", s, a, b);
        }
    };
    ($a:expr, $b:expr, $fmt:expr) => {
        {
            let a = $a;
            let b = $b;
            let s = stringify!($a == $b);
            alloc_assert!(a == b, concat!("{} (evaluated to {:?} == {:?}): ", $fmt), s, a, b);
        }
    };
    ($a:expr, $b:expr, $fmt:expr, $($arg:tt)*) => {
        {
            let a = $a;
            let b = $b;
            let s = stringify!($a == $b);
            alloc_assert!(a == b, concat!("{} (evaluated to {:?} == {:?}): ", $fmt), s, a, b, $($arg)*);
        }
    }
}

#[macro_export]
macro_rules! alloc_debug_assert_eq {
    ($($arg:tt)*) => {
        if cfg!(debug_assertions) {
            alloc_assert_eq!($($arg)*);
        }
    }
}

#[macro_export]
macro_rules! alloc_assert_ne {
    ($a:expr, $b:expr) => {
        {
            let a = $a;
            let b = $b;
            let s = stringify!($a != $b);
            alloc_assert!(a != b, "{} (evaluated to {:?} != {:?})", s, a, b);
        }
    };
    ($a:expr, $b:expr, $fmt:expr) => {
        {
            let a = $a;
            let b = $b;
            let s = stringify!($a != $b);
            alloc_assert!(a != b, concat!("{} (evaluated to {:?} != {:?}): ", $fmt), s, a, b);
        }
    };
    ($a:expr, $b:expr, $fmt:expr, $($arg:tt)*) => {
        {
            let a = $a;
            let b = $b;
            let s = stringify!($a != $b);
            alloc_assert!(a != b, concat!("{} (evaluated to {:?} != {:?}): ", $fmt), s, a, b, $($arg)*);
        }
    }
}

#[macro_export]
macro_rules! alloc_debug_assert_ne {
    ($($arg:tt)*) => {
        if cfg!(debug_assertions) {
            alloc_assert_ne!($($arg)*);
        }
    }
}

/// Types that can be unwrapped in an allocation-safe manner.
///
/// [`AllocUnwrap`] provides the [`alloc_unwrap`] and [`alloc_expect`] methods, which are allocation-safe
/// equivalents of the [`unwrap`][Option::unwrap] and [`expect`][Option::expect] methods on [`Option`] and [`Result`]. [`AllocUnwrap`] is
/// implemented for [`Option`] and [`Result`].
///
/// [`AllocUnwrap`]: trait.AllocUnwrap.html
/// [`alloc_unwrap`]: trait.AllocUnwrap.html#tymethod.alloc_unwrap
/// [`alloc_expect`]: trait.AllocUnwrap.html#tymethod.alloc_expect
///
/// # Examples
///
/// ```rust
/// use alloc_fmt::AllocUnwrap;
/// println!("{}", Some(1).alloc_unwrap());
/// ```
pub trait AllocUnwrap {
    type Item;
    fn alloc_unwrap(self) -> Self::Item;
    fn alloc_expect(self, msg: &str) -> Self::Item;
}

// The implementations for Option and Result are adapted from the Rust standard library.
impl<T> AllocUnwrap for Option<T> {
    type Item = T;

    #[inline]
    fn alloc_unwrap(self) -> T {
        match self {
            Some(val) => val,
            None => alloc_panic!("called `Option::alloc_unwrap()` on a `None` value"),
        }
    }

    #[inline]
    fn alloc_expect(self, msg: &str) -> T {
        // This is a separate function to reduce the code size of alloc_expect itself
        #[inline(never)]
        #[cold]
        fn failed(msg: &str) -> ! {
            alloc_panic!("{}", msg);
        }

        match self {
            Some(val) => val,
            None => failed(msg),
        }
    }
}

impl<T, E: ::core::fmt::Debug> AllocUnwrap for Result<T, E> {
    type Item = T;

    #[inline]
    fn alloc_unwrap(self) -> T {
        match self {
            Ok(val) => val,
            Err(err) => {
                result_unwrap_failed("called `Result::alloc_unwrap()` on an `Err` value", err)
            }
        }
    }

    #[inline]
    fn alloc_expect(self, msg: &str) -> T {
        match self {
            Ok(val) => val,
            Err(err) => result_unwrap_failed(msg, err),
        }
    }
}

// This is a separate function to reduce the code size of alloc_{expect,unwrap}
#[inline(never)]
#[cold]
fn result_unwrap_failed<E: ::core::fmt::Debug>(msg: &str, err: E) -> ! {
    alloc_panic!("{}: {:?}", msg, err)
}

/// Print a backtrace and then abort the process.
///
/// `print_backtrace_and_abort` should be called after any relevant output has been flushed to
/// stderr so that even if this function crashes (since the `backtrace` crate does not guarantee
/// allocation-free backtraces), as much information as possible has already been output.
#[doc(hidden)]
pub unsafe fn print_backtrace_and_abort() -> ! {
    // TODO(joshlf): Currently, this function prints itself and its callees in the trace. We should
    // figure out a way to omit those and have the first printed frame be the caller's.

    #[cfg(feature = "print-backtrace")]
    {
        backtrace::trace(|frame| {
            let ip = frame.ip();
            backtrace::resolve(ip, |symbol| {
                if let Some(name) = symbol.name() {
                    alloc_eprintln!("{}", name);
                } else {
                    alloc_eprintln!("<unknown function>");
                }
                if let Some(path) = symbol.filename() {
                    if let Some(s) = path.to_str() {
                        alloc_eprint!("\t{}", s);
                    } else {
                        alloc_eprint!("\t<unknown file>");
                    }
                } else {
                    alloc_eprint!("\t<unknown file>");
                }
                if let Some(line) = symbol.lineno() {
                    alloc_eprintln!(":{}", line);
                } else {
                    alloc_eprintln!();
                }
            });
            true
        });
    }
    core::intrinsics::abort();
}

// Test the macros by expanding them here and ensuring that they compile properly.
#[allow(unused)]
#[cfg_attr(feature = "cargo-clippy", allow(cyclomatic_complexity))]
fn never_called() {
    alloc_print!("foo");
    alloc_println!("foo");
    alloc_eprint!("foo");
    alloc_eprintln!("foo");
    alloc_assert!(false && true);
    alloc_assert!(false && true, "foo");
    alloc_assert!(false && true, "foo: {}", "bar");
    alloc_debug_assert!(false && true);
    alloc_debug_assert!(false && true, "foo");
    alloc_debug_assert!(false && true, "foo: {}", "bar");
    alloc_assert_eq!(1 + 2, 1);
    alloc_assert_eq!(1 + 2, 1, "foo");
    alloc_assert_eq!(1 + 2, 1, "foo: {}", "bar");
    alloc_debug_assert_eq!(1 + 2, 1);
    alloc_debug_assert_eq!(1 + 2, 1, "foo");
    alloc_debug_assert_eq!(1 + 2, 1, "foo: {}", "bar");
    alloc_assert_eq!(1 + 2, 3);
    alloc_assert_eq!(1 + 2, 3, "foo");
    alloc_assert_eq!(1 + 2, 3, "foo: {}", "bar");
    alloc_debug_assert_eq!(1 + 2, 3);
    alloc_debug_assert_eq!(1 + 2, 3, "foo");
    alloc_debug_assert_eq!(1 + 2, 3, "foo: {}", "bar");

    Some(0).alloc_unwrap();
    Some(0).alloc_expect("None");
    let _: usize = None.alloc_unwrap();
    let _: usize = None.alloc_expect("None");
    (Ok(0) as Result<_, &'static str>).alloc_unwrap();
    (Ok(0) as Result<_, &'static str>).alloc_expect("None");
    (Err("") as Result<usize, _>).alloc_unwrap();
    (Err("") as Result<usize, _>).alloc_expect("None");
}
