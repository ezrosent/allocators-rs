<!-- Copyright 2017 the authors. See the 'Copyright and license' section of the
README.md file at the top-level directory of this repository.

Licensed under the Apache License, Version 2.0 (the LICENSE-APACHE file) or
the MIT license (the LICENSE-MIT file) at your option. This file may not be
copied, modified, or distributed except according to those terms. -->

elfmalloc
=========

_Efficient dynamic memory allocation_

[![Crates.io](https://img.shields.io/crates/v/elfmalloc.svg)](https://crates.io/crates/elfmalloc)
[![Docs](https://docs.rs/elfmalloc/badge.svg)](https://docs.rs/elfmalloc)

This crate provides efficient multi-threaded heap allocation both on a
per-object (i.e. fixed-size) or dynamic (i.e. `malloc`-like) basis.
Most details are provided in the crate documentation.

Note, if you link in this crate to a Rust project (e.g. to use object-specific
allocators), you will want to set the `use_default_allocator` feature. Without
this feature, all existing dynamic allocation requests from the rest of the
project will be slower.

To compile a dynamic library that can be loaded in existing C programs, use the
[`elfc`](https://github.com/ezrosent/allocators-rs/tree/master/elfc) crate.

## Support

elfmalloc is currently in early alpha. Support on 64-bit Linux is relatively
complete, although there are certainly undiscovered bugs. 32-bit support is
theoretically sound, but completely untested. We are working towards 64-bit Mac
support, but even simple programs will currently encounter bugs or crashes on
Mac.

## More Info

- [Performance](https://github.com/ezrosent/allocators-rs/blob/master/info/elfmalloc-performance.md)
- [Design](https://github.com/ezrosent/allocators-rs/blob/master/info/elfmalloc.md)
