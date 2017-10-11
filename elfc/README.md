<!-- Copyright 2017 the authors. See the 'Copyright and license' section of the
README.md file at the top-level directory of this repository.

Licensed under the Apache License, Version 2.0 (the LICENSE-APACHE file) or
the MIT license (the LICENSE-MIT file) at your option. This file may not be
copied, modified, or distributed except according to those terms. -->

elfc
====

This crate uses [`malloc-bind`](https://crates.io/crates/malloc-bind) to provide
an implementation of the C allocation API (`malloc`, `free`, etc) based on the
[`elfmalloc`](https://crates.io/crates/elfmalloc) allocator. It compiles to a
dynamic library (`.so` on Linux and `.dylib` on Mac) that can be loaded using
the `LD_PRELOAD` or `DYLD_INSERT_LIBRARIES` environment variables (on Linux or
Mac respectively) or using `dlopen`.

elfmalloc is still in early alpha, and some platforms are only minimally
supported. For details on what's working and what isn't, see the elfmalloc
README.
