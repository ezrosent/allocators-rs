// Copyright 2017-2018 the authors. See the 'Copyright and license' section of the
// README.md file at the top-level directory of this repository.
//
// Licensed under the Apache License, Version 2.0 (the LICENSE-APACHE file) or
// the MIT license (the LICENSE-MIT file) at your option. This file may not be
// copied, modified, or distributed except according to those terms.

#![feature(alloc)]
#![feature(allocator_api)]
#![cfg_attr(test, feature(test))]
#![feature(thread_local_state)]
#![feature(thread_local)]
#![feature(const_fn)]
#![feature(const_size_of)]
#![feature(cfg_target_thread_local)]
#![feature(core_intrinsics)]
#![feature(const_ptr_null_mut)]
#![feature(nonnull_cast)]
extern crate alloc;
extern crate bagpipe;
extern crate crossbeam_epoch;
extern crate mmap_alloc;
extern crate num_cpus;
extern crate object_alloc;
extern crate sysconf;

#[macro_use]
extern crate alloc_fmt;
#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate log;
#[macro_use]
extern crate alloc_tls;

#[macro_use]
mod stats;
pub mod alloc_impl;
mod alloc_map;
mod alloc_type;
mod backing;
pub mod frontends;
pub mod general;
mod object;
// pub mod rust_alloc;
mod slag;
// mod sources;
mod util;
// pub mod vec_alloc;
