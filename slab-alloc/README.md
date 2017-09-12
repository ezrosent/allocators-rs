<!-- Copyright 2017 the authors. See the 'Copyright and license' section of the
README.md file at the top-level directory of this repository.

Licensed under the Apache License, Version 2.0 (the LICENSE-APACHE file) or
the MIT license (the LICENSE-MIT file) at your option. This file may not be
copied, modified, or distributed except according to those terms. -->

slab-alloc
==========

[![Crates.io](https://img.shields.io/crates/v/slab-alloc.svg)](https://crates.io/crates/slab-alloc)
[![Docs](https://docs.rs/slab-alloc/badge.svg)](https://docs.rs/slab-alloc)

The slab allocator. This crate implements an allocator whose design is based on Jeff Bonwick's [The Slab Allocator: An Object-Caching Kernel Memory Allocator](http://www.usenix.org/publications/library/proceedings/bos94/full_papers/bonwick.ps).

The slab allocator is an object allocator - it allocates and caches objects of a fixed type, and provides performance improvements over a general-purpose allocator. The allocator types in this crate implement the `ObjectAlloc` and `UntypedObjectAlloc` traits defined in the object-alloc crate. The slab allocator implemented in this crate is currently single-threaded and cannot be accessed concurrently.
