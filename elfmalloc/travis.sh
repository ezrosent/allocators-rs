#!/bin/bash

# Copyright 2017-2018 the authors. See the 'Copyright and license' section of the
# README.md file at the top-level directory of this repository.
#
# Licensed under the Apache License, Version 2.0 (the LICENSE-APACHE file) or
# the MIT license (the LICENSE-MIT file) at your option. This file may not be
# copied, modified, or distributed except according to those terms.

set -x
set -e

# Since elfmalloc tests use a lot of memory, we use RUST_TEST_THREADS=1
# to force them to run sequentially in order to reduce the total memory
# consumption and avoid a crash on Travis.

travis-cargo --only nightly build
RUST_TEST_THREADS=1 RUST_BACKTRACE=1 travis-cargo --only nightly test \
  --release -- --features low-memory-tests
for feature in    \
  prime_schedules \
  huge_segments   \
  no_lazy_region  \
  local_cache     \
  magazine_layer  \
  c-api; do
  RUST_TEST_THREADS=1 RUST_BACKTRACE=1 travis-cargo --only nightly test \
    --release -- --features "$feature low-memory-tests"
done
