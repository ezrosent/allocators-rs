#!/bin/bash

# Copyright 2017-2018 the authors. See the 'Copyright and license' section of the
# README.md file at the top-level directory of this repository.
#
# Licensed under the Apache License, Version 2.0 (the LICENSE-APACHE file) or
# the MIT license (the LICENSE-MIT file) at your option. This file may not be
# copied, modified, or distributed except according to those terms.

set -x
set -e

export RUST_TEST_THREADS=1

travis-cargo --only nightly build
# TODO: Figure out why test_map_panic_too_large results in SIGBUS
# (e.g., see https://travis-ci.org/ezrosent/allocators-rs/jobs/291713981)
# TODO: Remove -q and --verbose once the following issue is fixed:
# https://github.com/huonw/travis-cargo/issues/75
for feature in '' large-align; do
  RUST_BACKTRACE=1 travis-cargo -q --only nightly test -- --features "$feature" \
    --verbose -- --skip test_map_panic_too_large  
done
