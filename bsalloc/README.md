<!-- Copyright 2017 the authors. See the 'Copyright and license' section of the
README.md file at the top-level directory of this repository.

Licensed under the Apache License, Version 2.0 (the LICENSE-APACHE file) or
the MIT license (the LICENSE-MIT file) at your option. This file may not be
copied, modified, or distributed except according to those terms. -->

bsalloc
=======

[![Crates.io](https://img.shields.io/crates/v/bsalloc.svg)](https://crates.io/crates/bsalloc)
[![Docs](https://docs.rs/bsalloc/badge.svg)](https://docs.rs/bsalloc)

This crate implements a very simple global allocator. This is used to
service heap allocations for dependencies of the dynamic allocators in
the rest of the repo. In other words, this is used to **b**oot**s**trap
the more efficient allocators in this repo without creating a needless
dependency on another malloc implementation.

## Structure

The allocator is a thin wrapper around `mmap`. Instead of simply calling
`mmap` and `munmap` for dynamic allocations directly (this did slow
things down in some microbenchmarks), we have two "size classes",
each with a fixed-size global cache. These caches have a thread-safe
interface that allows a thread to efficiently re-use memory freed by it or
another thread. Naturally, allocations that are larger than the largest
size class fall back on `mmap` directly.
