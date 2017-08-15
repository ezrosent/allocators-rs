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
#[cfg(not(feature = "use_jemalloc"))]
extern crate bsalloc;
extern crate alloc;

#[macro_use]
mod macros;
mod utils;
pub mod slag;
pub mod general;
