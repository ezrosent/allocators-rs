<!-- Copyright 2017 the authors. See the 'Copyright and license' section of the
README.md file at the top-level directory of this repository.

Licensed under the Apache License, Version 2.0 (the LICENSE-APACHE file) or
the MIT license (the LICENSE-MIT file) at your option. This file may not be
copied, modified, or distributed except according to those terms. -->

alloc-tls
=========

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

Known limitations:
- `alloc-tls` does not currently support platforms that do not support the
  `#[thread_local]` attribute.
