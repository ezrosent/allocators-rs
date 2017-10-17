<!-- Copyright 2017 the authors. See the 'Copyright and license' section of the
README.md file at the top-level directory of this repository.

Licensed under the Apache License, Version 2.0 (the LICENSE-APACHE file) or
the MIT license (the LICENSE-MIT file) at your option. This file may not be
copied, modified, or distributed except according to those terms. -->

alloc-fmt
=========

`alloc-fmt` provides formatting and assertion macros similar to `println`,
`eprintln`, `panic`, `assert`, `debug_assert`, etc which are safe for use in a
global allocator. The standard library's formatting, panic, and assertion macros
can allocate, meaning that if they are used in the implementation of a global
allocator, it can cause infinite recursion. The macros in this crate do not
allocate in order to avoid this problem.
