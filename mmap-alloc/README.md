<!-- Copyright 2017 the authors. See the 'Copyright and license' section of the README.md file at the top-level directory of this repository.

Licensed under the Apache License, Version 2.0 (the LICENSE file). This file may not be copied, modified, or distributed except according to those terms. -->

mmap-alloc
==========

An allocator that is backed by directly mapping memory pages.

The `MapAlloc` type defined by this crate implements the `Alloc` and `ObjectAlloc` traits by directly mapping memory pages from the kernel (`mmap`/`munmap` on POSIX systems and `VirtualAlloc`/`VirtualFree` on Windows). It also allows pages to be committed or uncommitted.
