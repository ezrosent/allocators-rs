<!-- Copyright 2017 the authors. See the 'Copyright and license' section of the
README.md file at the top-level directory of this repository.

Licensed under the Apache License, Version 2.0 (the LICENSE-APACHE file) or
the MIT license (the LICENSE-MIT file) at your option. This file may not be
copied, modified, or distributed except according to those terms. -->

Design of alloc-tls
===================

`alloc-tls` provides the `alloc_thread_local!` macro, a near-drop-in replacement
for the standard library's `thread_local!` macro that is safe for use in
implementing a global allocator.

Unlike `thread_local!`, `alloc_thread_local!` address the following issues
unique to implementing a global allocator:
- On platforms that support the `#[thread_local]` attribute, registering
  destructors for types that implement `Drop` requires allocation. When a
  thread-local is initialized from a call to an allocation function (`malloc`,
  `free`, etc), this causes reentrancy. `alloc_thread_local!` can detect this.
- On Mac, it is not safe to access TLS while a dynamic library is being loaded.
  When implementing a Mac dynamic library that provides a global allocator,
  `alloc_thread_local!` can detect whether the library has been loaded or not,
  and can avoid using TLS if `malloc` or other similar calls are made from the
  loader itself during loading.

# Reentrancy

Reentrancy is addressed by expanding the number of states that a thread-local
variable can be in. Variables defined using the standard library's
`thread_local!` macro can be in one of three states - Uninitialized, Valid, and
Destroyed. In contrast, variables defined using `alloc_thread_local!` can be in
Uninitialized, Initializing (a new state), Initialized (equivalent to Valid), or
Dropped (equivalent to Destroyed). When a variable is accessed in the
Uninitialized state, it is moved into the Initializing state _before_ any
destructors are registered. This way, if registering destructors causes
allocation, any TLS access in that allocation will find the variable in the
Initializing state, and will thus be able to detect the reentrancy.

# Mac dynamic libraries

On Mac, dynamic libraries can specify library constructors - functions that are
called immediately after the library is loaded. When compiling a dynamic library
for Mac, `alloc-tls` defines a global `DYLD_LOADED` variable that is initialized
to false. A constructor is registered that sets it to true. When a TLS variable
is accessed, if `DYLD_LOADED` is false, the access fails, leaving it up to the
caller to use some slow path that doesn't rely on thread-local values.
