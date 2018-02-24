// Copyright 2017-2018 the authors. See the 'Copyright and license' section of the
// README.md file at the top-level directory of this repository.
//
// Licensed under the Apache License, Version 2.0 (the LICENSE-APACHE file) or
// the MIT license (the LICENSE-MIT file) at your option. This file may not be
// copied, modified, or distributed except according to those terms.

//! Allocator-safe thread-local storage.
//!
//! `alloc-tls` implements thread-local storage that, unlike the standard library's
//! implementation, is safe for use in a global allocator.

#![feature(allow_internal_unsafe)]
#![feature(const_fn)]
#![feature(const_ptr_null_mut)]
#![feature(const_unsafe_cell_new)]
#![feature(core_intrinsics)]
#![feature(fn_must_use)]
#![feature(test)]
#![feature(thread_local)]

#[macro_use]
extern crate alloc_fmt;

use std::cell::UnsafeCell;
use std::mem;
use std::ptr;

/// Declare a thread-local variable.
///
/// `alloc_thread_local` declares a thread-local variable which is safe for use in implementing a
/// global allocator. It is invoked as:
///
/// ```rust,ignore
/// alloc_thread_local!{ static <name>: <type> = <expr>; }
/// ```
///
/// For example,
///
/// ```rust
/// # #![feature(thread_local)]
/// # #[macro_use] extern crate alloc_tls;
/// # fn main() {
/// alloc_thread_local!{ static FOO: usize = 0; }
/// # }
/// ```
///
/// Thread-local variables follow a distinct lifecycle, and can be in one of four states:
/// - All thread-local variables start out as *uninitialized*.
/// - When a thread-local variable is first accessed, it is moved into the *initializing* state,
///   and its initializer is called.
/// - Once the initializer returns, the thread-local variable is initialized to the returned value,
///   and it moves into the *initialized* state.
/// - When the thread exits, the variable moves into the *dropped* state, and the variable is
///   dropped.
///
/// Thread-local variables can be accessed using the `with` method. If the variable is in the
/// *uninitialized* or *initialized* states, the variable can be accessed. Otherwise, it cannot,
/// and it is the caller's responsibility to figure out a workaround for its task that does not
/// involve accessing the thread-local variable.
///
/// Note that this macro uses the `#[thread_local]` attribute; in order to use it, you must put
/// `#![feature(thread_local)]` at the root of your crate.
#[macro_export]
#[allow_internal_unsafe]
macro_rules! alloc_thread_local {
    (static $name:ident: $t: ty = $init:expr;) => (
        #[thread_local]
        static $name: $crate::TLSSlot<$t> = {
            fn __init() -> $t { $init }

            unsafe fn __drop() { $name.drop(); }

            thread_local!{ static DROPPER: $crate::CallOnDrop = unsafe { $crate::CallOnDrop::new(__drop) }; }

            // DROPPER will only be dropped if it is first initialized, so we provide this function
            // to be called when the TLSSlot is first initialized. The act of calling DROPPER.with
            // will cause DROPPER to be initialized, ensuring that it will later be dropped on
            // thread exit.
            fn __register_dtor() { DROPPER.with(|_| {}); }

            $crate::TLSSlot::new(__init, __register_dtor)
        };
    )
}

#[derive(Eq, PartialEq)]
enum TLSValue<T> {
    Uninitialized,
    Initializing,
    Initialized(T),
    Dropped,
}

#[derive(PartialEq, Eq, Debug, Copy, Clone)]
enum TLSState {
    Uninitialized,
    Initializing,
    Initialized,
    Dropped,
}

impl<T> TLSValue<T> {
    fn state(&self) -> TLSState {
        match self {
            &TLSValue::Uninitialized => TLSState::Uninitialized,
            &TLSValue::Initializing => TLSState::Initializing,
            &TLSValue::Initialized(_) => TLSState::Initialized,
            &TLSValue::Dropped => TLSState::Dropped,
        }
    }
}

// Make likely available to the alloc_tls_fast_with macro.
#[doc(hidden)]
pub use std::intrinsics::likely;

/// Access the TLS slot with maximum performance.
///
/// `alloc_tls_fast_with` is the macro version of `TLSSlot`'s `with` method. In practice, we have
/// found that that method is not always optimized as much as it could be, and using a macro is
/// friendlier to the optimizer.
///
/// `$slot` is the `TLSSlot` to be accessed. `$blk` is a block of code that will be executed,
/// and the TLS value will be bound to a variable named `$name` in that block.
///
/// Note that this macro uses core intrinsics; in order to use it, you must put
/// `#![feature(core_intrinsics)]` at the root of your crate.
///
/// # Safety
///
/// `alloc_tls_fast_with` must be called from an `unsafe` block. It is unsafe because if `f`
/// panics, it causes undefined behavior.
///
/// # Example
///
/// ```rust
/// # #![feature(core_intrinsics)]
/// # #![feature(thread_local)]
/// # #[macro_use] extern crate alloc_tls;
/// # fn main() {
/// alloc_thread_local!{ static FOO: usize = 0; }
/// unsafe {
///     alloc_tls_fast_with!(FOO, foo, {
///         println!("foo: {}", foo);
///     });
/// }
/// # }
/// ```
#[macro_export]
macro_rules! alloc_tls_fast_with {
    ($slot:expr, $name:ident, $blk:block) => {
        if $crate::likely(!(*$slot.ptr.get()).is_null()) {
            let $name = &**$slot.ptr.get();
            Some($blk)
        } else {
            $slot.with_slow(|$name| {
                // ensure that type inference on $name succeeds regardless of the contents of $blk
                if $name as *const _ == &**$slot.ptr.get() as *const _ {}
                $blk
            })
        }
    };
}

/// A slot for a thread-local variable.
///
/// A `TLSSlot` should be initialized using the `internal_thread_local!` macro. See its
/// documentation for details on declaring and using thread-local variables.
pub struct TLSSlot<T> {
    // TODO: Use repr(C) to ensure that this field comes first so that we don't need to do extra
    // offset math to access it?

    // This field is a pointer to the T in slot (in state Initialized) or NULL (in any other
    // state). This allows us to make the fast path a single pointer comparison, which is faster in
    // practice than matching on a four-variant enum.
    #[doc(hidden)]
    pub ptr: UnsafeCell<*const T>,
    // The actual value itself.
    slot: UnsafeCell<TLSValue<T>>,
    init: fn() -> T,
    register_dtor: fn(),
}

impl<T> TLSSlot<T> {
    #[doc(hidden)]
    pub const fn new(init: fn() -> T, register_dtor: fn()) -> TLSSlot<T> {
        TLSSlot {
            slot: UnsafeCell::new(TLSValue::Uninitialized),
            ptr: UnsafeCell::new(ptr::null_mut()),
            init,
            register_dtor,
        }
    }

    /// Access the TLS slot.
    ///
    /// `with` accepts a function that will be called with a reference to the TLS value. If the
    /// slot is in the *initializing* or *dropped* state, `with` will return `None` without
    /// invoking `f`. If the slot is in the *uninitialized* state, `with` will initialize the value
    /// and then call `f`. If the slot is in the *initialized* state, `with` will call `f`. In
    /// either of these last two cases, `with` will return `Some(r)`, where `r` is the value
    /// returned from the call to `f`.
    ///
    /// # Safety
    /// `with` is unsafe because if `f` panics, it causes undefined behavior.
    #[inline]
    pub unsafe fn with<R, F: FnOnce(&T) -> R>(&self, f: F) -> Option<R> {
        // NOTE: We originally just had dyld_loaded hard-coded to return false when not compiling
        // for a Mac dylib, but we discovered that the unlikely intrinsic is opaque to the
        // optimizer, and so the if branch wasn't getting optimized out.
        #[cfg(all(feature = "dylib", target_os = "macos"))]
        {
            use std::intrinsics::unlikely;
            if unlikely(!dyld_loaded()) {
                return None;
            }
        }

        if likely(!(*self.ptr.get()).is_null()) {
            let ptr = *self.ptr.get();
            Some(f(&*ptr))
        } else {
            self.with_slow(f)
        }
    }

    // Use #[cold] to make it more likely that LLVM won't inline the call to with_slow in with,
    // which would bloat the instruction cache.
    #[doc(hidden)]
    #[cold]
    pub unsafe fn with_slow<R, F: FnOnce(&T) -> R>(&self, f: F) -> Option<R> {
        let ptr = self.slot.get();
        match &*ptr {
            // this branch should never be taken because if we're in state Initialized, then
            // self.ptr should be non-NULL, so we should have taken the fast path in with.
            &TLSValue::Initialized(_) => unreachable!(),
            &TLSValue::Uninitialized => {
                // Move into to the Initializing state before registering the destructor in
                // case registering the destructor involves allocation. If it does, the nested
                // access to this TLS value will detect that the value is in state
                // Initializing, the call to with will return None, and a fallback path can be
                // taken.
                *ptr = TLSValue::Initializing;
                *ptr = TLSValue::Initialized((self.init)());
                if let &TLSValue::Initialized(ref t) = &*ptr {
                    *self.ptr.get() = t as *const _;
                }
                (self.register_dtor)();
                self.with(f)
            }
            &TLSValue::Initializing | &TLSValue::Dropped => return None,
        }
    }

    #[doc(hidden)]
    pub unsafe fn drop(&self) {
        let state = (&*self.slot.get()).state();
        alloc_assert!(
            state == TLSState::Uninitialized || state == TLSState::Initialized,
            "TLSValue dropped while in state {:?}",
            state
        );

        // TODO: Figure out why it's possible to be dropped in state Uninitialized.
        if state == TLSState::Uninitialized {
            return;
        }

        alloc_assert!(
            !(*self.ptr.get()).is_null(),
            "null ptr in state: {:?}",
            state
        );

        // According to a comment in the standard library, "The macOS implementation of TLS
        // apparently had an odd aspect to it where the pointer we have may be overwritten
        // while this destructor is running. Specifically if a TLS destructor re-accesses TLS
        // it may trigger a re-initialization of all TLS variables, paving over at least some
        // destroyed ones with initial values. This means that if we drop a TLS value in place
        // on macOS that we could revert the value to its original state halfway through the
        // destructor, which would be bad!" -
        // https://github.com/rust-lang/rust/blob/master/src/libstd/sys/unix/fast_thread_local.rs
        //
        // Thus, it's important that we use mem::replace here. That way, the value is brought
        // into tmp and then dropped while it is a local variable, avoiding this problem.
        let tmp = mem::replace(&mut *self.slot.get(), TLSValue::Dropped);
        *self.ptr.get() = ptr::null_mut();
        mem::drop(tmp);
    }
}

unsafe impl<T> Sync for TLSSlot<T> {}

// The mechanics of registering destructors is complicated and involves a lot of cross-platform
// logic. Instead of implementing that all ourselves, we piggy back on the standard library's
// TLS implementation. Each TLSSlot has a corresponding LocalKey (from the standard library) whose
// value is a CallOnDrop holding a function which will invoke the drop method on the TLSSlot. This
// function is called in CallOnDrop's Drop implementation.
#[doc(hidden)]
pub struct CallOnDrop(unsafe fn());

impl CallOnDrop {
    // new is unsafe because constructing a CallOnDrop will cause f to be called when it is
    // dropped, so if new weren't unsafe, it would provide a way for safe code to invoke unsafe
    // code without an unsafe block.
    pub unsafe fn new(f: unsafe fn()) -> CallOnDrop {
        CallOnDrop(f)
    }
}

impl Drop for CallOnDrop {
    fn drop(&mut self) {
        unsafe {
            (self.0)();
        }
    }
}

// TODO: Modify this comment to include links to relevant docs/issues

// On Mac, TLS cannot be accessed while a dynamic library is being loaded (at least, that's what it
// appears from our own experimentation with DYLD_INSERT_LIBRARIES). Unfortunately, the code is
// used to load dynamic libraries performs allocations. Thus, when producing a Mac dynamic library
// (.dylib), we need to be able to detect whether we're being called from the loader itself. We
// accomplish this by using a global static (DYLD_LOADED) that indicates whether we've been loaded,
// and setting it to true in a library constructor (dyld_init).

#[cfg(all(feature = "dylib", target_os = "macos"))]
static mut DYLD_LOADED: bool = false;

#[cfg(all(feature = "dylib", target_os = "macos"))]
fn dyld_loaded() -> bool {
    unsafe { DYLD_LOADED }
}

// On Mac, the C ABI prefixes all symbols with _, so use the symbol name _dyld_init instead of
// dyld_init. Source: https://users.rust-lang.org/t/ld-preload-init-function-in-rust/12865/6
// TODO: Consider switching to using the .mod_init_funcs (e.g.,
// #[link_secction = ".mod_init_funcs"]) as recommended here:
// https://community.embarcadero.com/blogs/entry/mac-os-x-shared-library-initialization-5639

// TODO: #[must_use] doesn't seem to work here. Is there a way we can ensure compilation or link
// failure if dyld_init isn't linked as the constructor (or at least isn't used in some way)?

/// Dynamic load initializer.
///
/// While compiling a dynamic library on Mac, this function must be registered as a library
/// constructor. The top-level crate must include the following linker directive:
/// `#![cfg(link_args = "-Wl,-init,_dyld_init")]`.
///
/// Alternatively, if a library constructor is already used, place a call to this function as the
/// first line of that constructor.
#[cfg(all(feature = "dylib", target_os = "macos"))]
#[must_use]
#[no_mangle]
pub extern "C" fn dyld_init() {
    // TODO: Remove once elfmalloc Mac support is completed
    alloc_eprintln!("alloc-tls: dyld loaded");
    unsafe {
        DYLD_LOADED = true;
    }
}

#[cfg(test)]
mod tests {
    // Modified from the Rust standard library

    extern crate test;
    use std::sync::mpsc::{channel, Sender};
    use std::cell::UnsafeCell;
    use std::thread;
    use super::*;
    use self::test::{black_box, Bencher};

    struct Foo(Sender<()>);

    impl Drop for Foo {
        fn drop(&mut self) {
            let Foo(ref s) = *self;
            s.send(()).unwrap();
        }
    }

    #[test]
    fn smoke_dtor() {
        alloc_thread_local!{ static FOO: UnsafeCell<Option<Foo>> = UnsafeCell::new(None); }

        let (tx, rx) = channel();
        let _t = thread::spawn(move || unsafe {
            let mut tx = Some(tx);
            FOO.with(|f| {
                *f.get() = Some(Foo(tx.take().unwrap()));
            });
        });
        rx.recv().unwrap();
    }

    #[test]
    fn lifecycle() {
        static mut DROPPED: bool = false;
        fn drop() {
            unsafe { DROPPED = true }
        }
        alloc_thread_local!{ static FOO: CallOnDrop = CallOnDrop(drop); }

        thread::spawn(|| unsafe {
            assert_eq!((&*FOO.slot.get()).state(), TLSState::Uninitialized);
            FOO.with(|_| {}).unwrap();
            assert_eq!((&*FOO.slot.get()).state(), TLSState::Initialized);
        }).join()
            .unwrap();
        assert_eq!(unsafe { DROPPED }, true);
    }

    #[bench]
    fn bench_tls(b: &mut Bencher) {
        alloc_thread_local!{ static FOO: UnsafeCell<usize> = UnsafeCell::new(0); }
        b.iter(|| unsafe {
            FOO.with(|foo| {
                let inner = foo.get();
                (*inner) += 1;
                black_box(*inner);
            });
        })
    }
}
