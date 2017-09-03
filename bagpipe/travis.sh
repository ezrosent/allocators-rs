#!/bin/bash

# Copyright 2017 the authors. See the 'Copyright and license' section of the
# README.md file at the top-level directory of this repository.
#
# Licensed under the Apache License, Version 2.0 (the LICENSE file). This file
# may not be copied, modified, or distributed except according to those terms.

set -x
set -e

travis-cargo build
RUST_BACKTRACE=1 travis-cargo test
for feature in check_empty_yq prime_schedules staggered_indexes huge_segments; do
  RUST_BACKTRACE=1 travis-cargo test -- --features "$feature"
done
