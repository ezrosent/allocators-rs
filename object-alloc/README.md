<!-- Copyright 2017 the authors. See the 'Copyright and license' section of the
README.md file at the top-level directory of this repository.

Licensed under the Apache License, Version 2.0 (the LICENSE-APACHE file) or
the MIT license (the LICENSE-MIT file) at your option. This file may not be
copied, modified, or distributed except according to those terms. -->

object-alloc
============

[![Crates.io](https://img.shields.io/crates/v/object-alloc.svg)](https://crates.io/crates/object-alloc)
[![Docs](https://docs.rs/object-alloc/badge.svg)](https://docs.rs/object-alloc)

Object allocators in Rust. This crate defines the `ObjectAlloc` trait and related types. An object allocator is an allocator which allocates and caches objects of a particular type, allowing for a number of performance improvements over general-purpose allocators that need to be able to service allocation requests of any size and alignment. Since all objects are of the same type, an object allocator can cache freed objects in a constructed state, and can thus allocate by re-using these cached objects. This allows object construction to be elided in certain circumstances, which can provide a further performance improvement.

This crate only defines types - it does not contain any implementations of the `ObjectAlloc` trait.
