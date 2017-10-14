// Copyright 2017 the authors. See the 'Copyright and license' section of the
// README.md file at the top-level directory of this repository.
//
// Licensed under the Apache License, Version 2.0 (the LICENSE-APACHE file) or
// the MIT license (the LICENSE-MIT file) at your option. This file may not be
// copied, modified, or distributed except according to those terms.

#[macro_use]
extern crate alloc_fmt;

#[cfg_attr(feature = "cargo-clippy", allow(cyclomatic_complexity))]
fn main() {
    let arg = std::env::args().nth(1).expect("must provide an argument");

    match arg.as_str() {
        "assert" => alloc_assert!(false && true),
        "assert_fmt" => alloc_assert!(false && true, "foo"),
        "assert_fmt_args" => alloc_assert!(false && true, "foo: {}", "bar"),
        "debug_assert" => alloc_debug_assert!(false && true),
        "debug_assert_fmt" => alloc_debug_assert!(false && true, "foo"),
        "debug_assert_fmt_args" => alloc_debug_assert!(false && true, "foo: {}", "bar"),
        "assert_eq" => alloc_assert_eq!(1 + 2, 1),
        "assert_eq_fmt" => alloc_assert_eq!(1 + 2, 1, "foo"),
        "assert_eq_fmt_args" => alloc_assert_eq!(1 + 2, 1, "foo: {}", "bar"),
        "debug_assert_eq" => alloc_debug_assert_eq!(1 + 2, 1),
        "debug_assert_eq_fmt" => alloc_debug_assert_eq!(1 + 2, 1, "foo"),
        "debug_assert_eq_fmt_args" => alloc_debug_assert_eq!(1 + 2, 1, "foo: {}", "bar"),
        "assert_ne" => alloc_assert_ne!(1 + 2, 3),
        "assert_ne_fmt" => alloc_assert_ne!(1 + 2, 3, "foo"),
        "assert_ne_fmt_args" => alloc_assert_ne!(1 + 2, 3, "foo: {}", "bar"),
        "debug_assert_ne" => alloc_debug_assert_ne!(1 + 2, 3),
        "debug_assert_ne_fmt" => alloc_debug_assert_ne!(1 + 2, 3, "foo"),
        "debug_assert_ne_fmt_args" => alloc_debug_assert_ne!(1 + 2, 3, "foo: {}", "bar"),
        _ => eprintln!("see source code for available arguments"),
    }
}
