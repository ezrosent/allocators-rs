// Copyright 2017-2018 the authors. See the 'Copyright and license' section of the
// README.md file at the top-level directory of this repository.
//
// Licensed under the Apache License, Version 2.0 (the LICENSE-APACHE file) or
// the MIT license (the LICENSE-MIT file) at your option. This file may not be
// copied, modified, or distributed except according to those terms.

#![no_std]
#![feature(alloc, allocator_api)]
#![feature(core_intrinsics)]
#![feature(nonnull_cast)]

extern crate alloc;
use alloc::allocator::Layout;
use core::intrinsics::abort;
use core::ptr::NonNull;

/// Allocators which allocate objects of a particular type.
///
/// `ObjectAlloc`s provide an interface which is slightly different than the interface provided by
/// a standard allocator. By definition, they are only capable of allocating objects of a
/// particular type. Additionally, memory returned from a call to `alloc` is guaranteed to already
/// be a valid, initialized instance of `T`. `ObjectAlloc`s may differ in how much flexibility they
/// provide in specifying how allocated objects are initialized, and `ObjectAlloc`s obtained using
/// `unsafe` constructors are allowed to break these initialization rules, allocating uninitialized
/// or invalid objects.
///
/// These differences allow `ObjectAlloc`s to provide significant performance improvements over
/// general-purpose allocators. First, only having to allocate objects of a particular size and
/// alignment allows them to make optimizations that are not available to general-purpose
/// allocators. Second, since `alloc` is required to return already-constructed objects, clients
/// don't have to initialize allocated objects. This, coupled with an object-caching scheme for
/// `dealloc`'d objects, allows many calls to `allloc` to avoid initialization altogether.
///
/// # Dropping
///
/// When an `ObjectAlloc` that allocates initialized objects is dropped, all cached `T` objects
/// that have not yet been dropped are dropped. The order in which they are dropped is undefined.
///
/// # Use in unsafe code
///
/// Unsafe code may rely on the fact that objects allocated by an `ObjectAlloc` are initialized.
/// Because of this, it must _not_ be possible for safe code to construct an `ObjectAlloc` that
/// doesn't properly initialize objects, or else safe code could construct such an `ObjectAlloc`,
/// pass it to a safe API that uses unsafe code under the hood (and that relies on the
/// initialization behavior of `ObjectAlloc`s), and thus cause memory unsafety.
///
/// It is acceptable for implementors to provide constructors that produce `ObjectAlloc`s that do
/// not abide by the initialization requirements, but these constructors must be `unsafe` so that
/// they cannot be called from safe code.
pub unsafe trait ObjectAlloc<T> {
    /// Allocates an object of type `T`.
    ///
    /// If this `ObjectAlloc` was obtained using a safe constructor (as opposed to an `unsafe`
    /// one), then the memory pointed to by the returned raw pointer is guaranteed to be a valid,
    /// initialized instance of `T`. In particular, the returned object will be in one of the
    /// following two states:
    ///
    /// * The result of a call to whatever initialization function was used to configure this
    ///   `ObjectAlloc`
    /// * The same state as a `T` which was previously returned via a call to `dealloc`
    ///
    /// On the other hand, if this `ObjectAlloc` was obtained using an `unsafe` constructor, then
    /// `alloc` may return uninitialized or invalid instances of `T` - the exact behavior should
    /// be documented in the constructor.
    ///
    /// The memory returned by `alloc` is guaranteed to be aligned according to the requirements of
    /// `T` (that is, according to `core::mem::align_of::<T>()`).
    unsafe fn alloc(&mut self) -> Option<NonNull<T>>;

    /// Deallocates an object previously returned by `alloc`.
    ///
    /// If `x` was not obtained through a call to `alloc`, or if `x` has already been `dealloc`'d,
    /// the behavior of `dealloc` is undefined.
    ///
    /// It is valid for `x` to be cached and used to serve future calls to `alloc`. The only
    /// guarantee that is made is that if this `ObjectAlloc` allocates initialized objects (unsafe
    /// constructors are allowed to produce `ObjectAlloc`s that do not allocate initialized
    /// objects), then `x` will be dropped at some point during the `ObjectAlloc`'s lifetime. This
    /// may happen during this call to `dealloc`, when the `ObjectAlloc` itself is dropped, or some
    /// time in between.
    unsafe fn dealloc(&mut self, x: NonNull<T>);

    /// Allocator-specific method for signalling an out-of-memory condition.
    ///
    /// `oom` aborts the thread or process, optionally performing cleanup or logging diagnostic
    /// information before panicking or aborting.
    ///
    /// `oom` is meant to be used by clients which are unable to cope with an unsatisfied
    /// allocation request, and wish to abandon computation rather than attempt to recover locally.
    /// The allocator likely has more insight into why the request failed, and thus can likely
    /// print more informative diagnostic information than the client could.
    ///
    /// Implementations of the `oom` method are discouraged from infinitely regressing in nested
    /// calls to `oom`. In practice this means implementors should eschew allocating, especially
    /// from `self` (directly or indirectly).
    ///
    /// Implementions of `alloc` are discouraged from panicking (or aborting) in the event of
    /// memory exhaustion; instead they should return an error and let the client decide whether to
    /// invoke this `oom` method in response.
    fn oom(&mut self) -> ! {
        unsafe { abort() }
    }
}

/// An allocator for objects whose type or size is not known at compile time.
///
/// `UntypedObjectAlloc` is like `ObjectAlloc`, except that the size that it allocates may be
/// configured at runtime. Also unlike `ObjectAlloc`, `UntypedObjectAlloc`s make no guarantees
/// about initialization of objects. An individual implementation of `UntypedObjectAlloc` may
/// decide to make such guarantees, but it is not required in order to be a correct implementation
/// of this trait, and the correctness of unsafe code must not rely on this behavior.
pub unsafe trait UntypedObjectAlloc {
    /// Obtains the `Layout` of allocated objects.
    ///
    /// `layout` returns a `Layout` object describing objects allocated by this
    /// `UntypedObjectAlloc`. All objects obtained via `alloc` are guaranteed to satisfy this
    /// `Layout`.
    fn layout(&self) -> Layout;

    /// Allocates a new object.
    ///
    /// The memory returned by `alloc` is guaranteed to abide by the `Layout` returned from
    /// `layout`.
    unsafe fn alloc(&mut self) -> Option<NonNull<u8>>;

    /// Deallocates an object previously returned by `alloc`.
    ///
    /// If `x` was not obtained through a call to `alloc`, or if `x` has already been `dealloc`'d,
    /// the behavior of `dealloc` is undefined.
    unsafe fn dealloc(&mut self, x: NonNull<u8>);

    /// Allocator-specific method for signalling an out-of-memory condition.
    ///
    /// `oom` aborts the thread or process, optionally performing cleanup or logging diagnostic
    /// information before panicking or aborting.
    ///
    /// `oom` is meant to be used by clients which are unable to cope with an unsatisfied
    /// allocation request, and wish to abandon computation rather than attempt to recover locally.
    /// The allocator likely has more insight into why the request failed, and thus can likely
    /// print more informative diagnostic information than the client could.
    ///
    /// Implementations of the `oom` method are discouraged from infinitely regressing in nested
    /// calls to `oom`. In practice this means implementors should eschew allocating, especially
    /// from `self` (directly or indirectly).
    ///
    /// Implementions of `alloc` are discouraged from panicking (or aborting) in the event of
    /// memory exhaustion; instead they should return an error and let the client decide whether to
    /// invoke this `oom` method in response.
    fn oom(&mut self) -> ! {
        unsafe { abort() }
    }
}

unsafe impl<T> UntypedObjectAlloc for ObjectAlloc<T> {
    fn layout(&self) -> Layout {
        // NOTE: This is safe because the layout method doesn't guarantee that it provides the most
        // specific layout, but rather simply that all objects returned from alloc are guaranteed
        // to satisfy by this layout. This particular ObjectAlloc could have been configured with a
        // more strict alignment than T's alignment, but that's OK.
        Layout::new::<T>()
    }

    unsafe fn alloc(&mut self) -> Option<NonNull<u8>> {
        ObjectAlloc::alloc(self).map(|x| x.cast())
    }

    unsafe fn dealloc(&mut self, x: NonNull<u8>) {
        ObjectAlloc::dealloc(self, x.cast());
    }
}
