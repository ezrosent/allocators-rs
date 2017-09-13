<!-- Copyright 2017 the authors. See the 'Copyright and license' section of the
README.md file at the top-level directory of this repository.

Licensed under the Apache License, Version 2.0 (the LICENSE-APACHE file) or
the MIT license (the LICENSE-MIT file) at your option. This file may not be
copied, modified, or distributed except according to those terms. -->

# Efficient Dynamic Memory Allocation

[![Crates.io](https://img.shields.io/crates/v/elfmalloc.svg)](https://crates.io/crates/elfmalloc)
[![Docs](https://docs.rs/elfmalloc/badge.svg)](https://docs.rs/elfmalloc)

This crate provides efficient multi-threaded heap allocation both on a
per-object (i.e. fixed-size) or dynamic (i.e. `malloc`-like) basis.
Most of the details are currently provided in the crate documentation.

Note that the allocators in this crate only work on 64-bit machines
right now. There are currently some ideas on how to add 32-bit support,
but any such changes would require serious additions to the allocators'
designs.

Note, if you link in this crate to a Rust project (e.g. to use object-specific
allocators), you will want to set the `use_default_allocator` feature. Without
this feature, all existing dynamic allocation requests from the rest of the
project will be slower.

## More Info

* [Performance](https://github.com/ezrosent/allocators-rs/blob/master/info/elfmalloc-performance.md)
* [Design](https://github.com/ezrosent/allocators-rs/blob/master/info/elfmalloc.md)
