// Copyright 2017-2018 the authors. See the 'Copyright and license' section of the
// README.md file at the top-level directory of this repository.
//
// Licensed under the Apache License, Version 2.0 (the LICENSE-APACHE file) or
// the MIT license (the LICENSE-MIT file) at your option. This file may not be
// copied, modified, or distributed except according to those terms.

#![feature(allocator_api)]
#![cfg_attr(test, feature(test))]
#![feature(thread_local)]
#![feature(const_fn)]
#![feature(cfg_target_thread_local)]
#![feature(core_intrinsics)]
#![feature(raw_vec_internals)]
extern crate alloc;
extern crate bagpipe;
extern crate mmap_alloc;
extern crate num_cpus;
extern crate sysconf;

#[macro_use]
extern crate alloc_fmt;
// Linking in `bsalloc` causes it to be used as the global heap allocator. That is important when
// using this as a basis for a `malloc` library, but it becomes a hindrance when using this crate
// as a specialized allocator library.
#[cfg(not(feature = "use_default_allocator"))]
extern crate bsalloc;
#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate log;
#[macro_use]
extern crate alloc_tls;

mod sources;
mod alloc_type;
mod utils;
#[macro_use]
mod stats;
mod slag;
pub mod frontends;
pub mod general;

pub mod alloc_impl;
pub mod rust_alloc;
pub mod vec_alloc;
