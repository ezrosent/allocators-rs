<!-- Copyright 2017 the authors. See the 'Copyright and license' section of the
README.md file at the top-level directory of this repository.

Licensed under the Apache License, Version 2.0 (the LICENSE-APACHE file) or
the MIT license (the LICENSE-MIT file) at your option. This file may not be
copied, modified, or distributed except according to those terms. -->

malloc-bind
===========

[![Crates.io](https://img.shields.io/crates/v/malloc-bind.svg)](https://crates.io/crates/malloc-bind)
[![Docs](https://docs.rs/malloc-bind/badge.svg)](https://docs.rs/malloc-bind)

The `malloc-bind` crate provides bindings for the C `malloc` API. Given an implementation of the Rust `Alloc` trait, it produces implementations of the C `malloc` API (`malloc`, `free`, `realloc`, etc) and defines `extern "C"` functions that can be used to produce a C shared object file.

## Platform support

`malloc-bind` currently supports Linux, Mac, and Windows.
