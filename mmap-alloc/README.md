<!-- Copyright 2017 the authors. See the 'Copyright and license' section of the
README.md file at the top-level directory of this repository.

Licensed under the Apache License, Version 2.0 (the LICENSE-APACHE file) or
the MIT license (the LICENSE-MIT file) at your option. This file may not be
copied, modified, or distributed except according to those terms. -->

mmap-alloc
==========

[![Crates.io](https://img.shields.io/crates/v/mmap-alloc.svg)](https://crates.io/crates/mmap-alloc)
[![Docs](https://docs.rs/mmap-alloc/badge.svg)](https://docs.rs/mmap-alloc)

An allocator that is backed by directly mapping memory pages.

The `MapAlloc` type defined by this crate implements the `Alloc` and `ObjectAlloc` traits by directly mapping memory pages from the kernel (`mmap`/`munmap` on POSIX systems and `VirtualAlloc`/`VirtualFree` on Windows). It also allows pages to be committed or uncommitted.
