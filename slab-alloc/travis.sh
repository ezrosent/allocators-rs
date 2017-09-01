#!/bin/bash

# Copyright 2017 the authors. See the 'Copyright and license' section of the
# README.md file at the top-level directory of this repository.
#
# Licensed under the Apache License, Version 2.0 (the LICENSE file). This file
# may not be copied, modified, or distributed except according to those terms.

set -x
set -e

travis-cargo --only nightly build
# Use opt-level=3 because the corruption tests take a long time, and the
# optimized build saves more time in test execution than it adds in compilation
# time.
RUSTFLAGS='-C opt-level=3' RUST_BACKTRACE=1 travis-cargo --only nightly test
# TODO: Once no-std and no-os work, add those features.
for feature in build-ignored-tests use-stdlib-hashmap no-coloring hashmap-no-resize hashmap-no-coalesce; do
  RUSTFLAGS='-C opt-level=3' RUST_BACKTRACE=1 travis-cargo --only nightly test -- \
  --features "$feature"
done
