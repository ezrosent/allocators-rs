// Copyright 2017-2018 the authors. See the 'Copyright and license' section of the
// README.md file at the top-level directory of this repository.
//
// Licensed under the Apache License, Version 2.0 (the LICENSE-APACHE file) or
// the MIT license (the LICENSE-MIT file) at your option. This file may not be
// copied, modified, or distributed except according to those terms.

#![feature(alloc)]
#![feature(allocator_api)]
#![cfg_attr(test, feature(test))]
#![feature(thread_local)]
#![feature(cfg_target_thread_local)]
#![feature(core_intrinsics)]
#![feature(raw_vec_internals)]
#![feature(specialization)]
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
mod rust_alloc;
mod slag;
mod util;
mod vec_alloc;

pub use object::{ElfBuilder, ElfUntypedObjectAlloc};
pub use rust_alloc::{ElfMalloc, GlobalAlloc};
pub use vec_alloc::AVec;