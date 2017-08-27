// Copyright 2017 the authors. See the 'Copyright and license' section of the
// README.md file at the top-level directory of this repository.
//
// Licensed under the Apache License, Version 2.0 (the LICENSE file). This file
// may not be copied, modified, or distributed except according to those terms.

#![feature(alloc)]
#![feature(allocator_api)]
#![cfg_attr(feature = "nightly", feature(thread_local_state))]
#![cfg_attr(feature = "nightly", feature(thread_local))]
#![cfg_attr(feature = "nightly", feature(const_fn))]
#![cfg_attr(feature = "nightly", feature(cfg_target_thread_local))]
#![cfg_attr(feature = "nightly", feature(core_intrinsics))]
#[macro_use]
extern crate lazy_static;
extern crate bagpipe;
extern crate bsalloc;
extern crate alloc;

#[macro_use]
mod macros;
mod utils;
#[macro_use]
mod stats;
pub mod slag;
pub mod general;
