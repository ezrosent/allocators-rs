// Copyright 2017-2018 the authors. See the 'Copyright and license' section of the
// README.md file at the top-level directory of this repository.
//
// Licensed under the Apache License, Version 2.0 (the LICENSE-APACHE file) or
// the MIT license (the LICENSE-MIT file) at your option. This file may not be
// copied, modified, or distributed except according to those terms.

#![feature(alloc)]
#![feature(allocator_api)]
#![feature(test)]
#![feature(const_fn)]
#![feature(const_size_of)]
#![feature(plugin)]
#![plugin(interpolate_idents)]

extern crate alloc;
#[cfg(unix)]
#[macro_use]
extern crate lazy_static;
#[cfg_attr(feature = "cargo-clippy", allow(useless_attribute))]
#[allow(plugin_as_library)]
extern crate interpolate_idents;
pub use interpolate_idents::*;
extern crate object_alloc;

pub mod corruption;
pub mod leaky_alloc;
pub mod types;

/// Call a function once for each alignment.
///
/// `foreach_align` calls `f` once for each valid alignment of `T` not greater than `max`. If `max`
/// is greater than any valid alignment of `T`, `f` is called for each valid alignment of `T`, and
/// `max` is ignored.
///
/// `foreach_align` is useful for testing allocators whose behavior may be sensitive to requested
/// alignment.
pub fn foreach_align<T, F: Fn(usize)>(f: F, max: usize) {
    use std::mem::{align_of, size_of};

    let min_align = align_of::<T>();
    let size = size_of::<T>();
    let mut align = min_align;
    while align <= size && align <= max {
        if size % align == 0 {
            f(align);
        }
        align *= 2;
    }
}
