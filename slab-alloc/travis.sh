#!/bin/bash

# Copyright 2017-2018 the authors. See the 'Copyright and license' section of the
# README.md file at the top-level directory of this repository.
#
# Licensed under the Apache License, Version 2.0 (the LICENSE-APACHE file) or
# the MIT license (the LICENSE-MIT file) at your option. This file may not be
# copied, modified, or distributed except according to those terms.

set -x
set -e

travis-cargo --only nightly build
# Use --release because the corruption tests take a long time, and the
# optimized build saves more time in test execution than it adds in compilation
# time.
# TODO: Now that we have opt-level = 3 in the test profile in Cargo.toml,
# is this necessary anymore?
RUST_BACKTRACE=1 travis-cargo --only nightly test -- --release
# TODO: Once no-std and no-os work, add those features.
for feature in build-ignored-tests use-stdlib-hashmap no-coloring hashmap-no-resize hashmap-no-coalesce; do
  RUST_BACKTRACE=1 travis-cargo --only nightly test -- --release --features "$feature"
done
