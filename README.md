<!-- Copyright 2017 the authors. See the 'Copyright and license' section of the
README.md file at the top-level directory of this repository.

Licensed under the Apache License, Version 2.0 (the LICENSE-APACHE file) or
the MIT license (the LICENSE-MIT file) at your option. This file may not be
copied, modified, or distributed except according to those terms. -->

# Allocators in Rust

[![Build Status](https://travis-ci.org/ezrosent/allocators-rs.svg?branch=master)](https://travis-ci.org/ezrosent/allocators-rs)
[![Build status](https://ci.appveyor.com/api/projects/status/github/ezrosent/allocators-rs?svg=true)](https://ci.appveyor.com/project/ezrosent/allocators-rs)

_Looking for elfmalloc in particular? It's [here](https://github.com/ezrosent/allocators-rs/blob/master/elfmalloc)._

This repository encompasses a number of different crates. Some are general
memory allocators, some are object allocators (allocators which allocate a
specific type of object), and some are utility crates providing various
features needed to implement these allocators.

The [`info`](https://github.com/ezrosent/allocators-rs/blob/master/info) directory contains more detailed information, including performance
measurements. Aside from `info`, all top-level directories are Rust crates.
More detailed information on each crate can be found in the crate's `README.md`.

### Allocator crates

| Crate | Description |
|-------|-------------|
| [`elfmalloc`](https://github.com/ezrosent/allocators-rs/blob/master/elfmalloc) | A general-purpose multi-threaded allocator providing both Rust `Alloc` and C `malloc` APIs |
| [`slab-alloc`](https://github.com/ezrosent/allocators-rs/blob/master/slab-alloc) | An object-specific slab allocator in the same tradition as the original [slab allocator](https://www.usenix.org/legacy/publications/library/proceedings/bos94/full_papers/bonwick.a) by Jeff Bonwick |
| [`bsalloc`](https://github.com/ezrosent/allocators-rs/blob/master/bsalloc) | A simple general-purpose "bootstrapping" allocator used in the implementation of other allocators |
| [`mmap-alloc`](https://github.com/ezrosent/allocators-rs/blob/master/mmap-alloc) | An `Alloc` API which is a thin wrapper around `mmap` (on Unix) or `VirtualAlloc` (on Windows) |

### Utility crates

| Crate | Description |
|-------|-------------|
| [`alloc-fmt`](https://github.com/ezrosent/allocators-rs/blob/master/alloc-fmt) | Allocation-safe formatting and debugging macros (unreleased) |
| [`bagpipe`](https://github.com/ezrosent/allocators-rs/blob/master/bagpipe) | Fast, concurrent data structures including queues and a weakly-ordered bag data structure ([design](https://github.com/ezrosent/allocators-rs/blob/master/info/bagpipes.md)) |
| [`malloc-bind`](https://github.com/ezrosent/allocators-rs/blob/master/malloc-bind) | Bindings to allow a Rust `Alloc` to implement the C `malloc` API |
| [`object-alloc`](https://github.com/ezrosent/allocators-rs/blob/master/object-alloc) | Traits representing type-specific variants of the `Alloc` trait |
| [`object-alloc-test`](https://github.com/ezrosent/allocators-rs/blob/master/object-alloc-test) | A correctness test suite for object allocator implementations (uses the traits defined in `object-alloc`) |

## Contributing

Interested in contributing? We'd love to have you! Check out [CONTRIBUTING.md](https://github.com/ezrosent/allocators-rs/blob/master/CONTRIBUTING.md).

## Copyright and license

Copyrights in this project are retained by their contributors. No copyright
assignment is required to contribute to this project. For a list of authors, see
this repository's version control history.

This project is dual-licensed under the  [Apache
2.0](http://www.apache.org/licenses/LICENSE-2.0) license or the
[MIT](http://opensource.org/licenses/MIT) license at your option. Copies of
these licenses can be found in the `LICENSE-APACHE` and `LICENSE-MIT` files
respectively.
