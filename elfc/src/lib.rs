// Copyright 2017-2018 the authors. See the 'Copyright and license' section of the
// README.md file at the top-level directory of this repository.
//
// Licensed under the Apache License, Version 2.0 (the LICENSE-APACHE file) or
// the MIT license (the LICENSE-MIT file) at your option. This file may not be
// copied, modified, or distributed except according to those terms.

#![cfg_attr(any(feature = "logging", target_os = "macos"), feature(link_args))]
#![cfg_attr(all(feature = "logging", target_os = "linux"), link_args = "-Wl,-init,init_log")]
// On Mac, the C ABI prefixes all symbols with _.
// Source: https://users.rust-lang.org/t/ld-preload-init-function-in-rust/12865/6
// TODO: Consider switching to using the .mod_init_funcs (e.g.,
// #[link_secction = ".mod_init_funcs"]) as recommended here:
// https://community.embarcadero.com/blogs/entry/mac-os-x-shared-library-initialization-5639
#![cfg_attr(all(feature = "logging", target_os = "macos"), link_args = "-Wl,-init,_init")]
#![cfg_attr(all(not(feature = "logging"), target_os = "macos"), link_args = "-Wl,-init,_dyld_init")]

#[cfg(feature = "logging")]
extern crate alloc_tls;
// Linking in `bsalloc` causes it to be used as the global heap allocator.
// This is important because otherwise any allocation performed inside of
// elfmalloc using the global allocator would be recursive.
extern crate bsalloc;
extern crate elfmalloc;
#[cfg(feature = "logging")]
extern crate env_logger;
#[macro_use]
extern crate malloc_bind;
use elfmalloc::alloc_impl::ElfMallocGlobal;

define_malloc!(ElfMallocGlobal, ElfMallocGlobal);

#[cfg(feature = "logging")]
#[no_mangle]
pub extern "C" fn init() {
    alloc_tls::dyld_init();
    let _ = env_logger::init();
}
