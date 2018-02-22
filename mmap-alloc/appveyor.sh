#!/bin/bash

# Copyright 2017-2018 the authors. See the 'Copyright and license' section of the
# README.md file at the top-level directory of this repository.
#
# Licensed under the Apache License, Version 2.0 (the LICENSE-APACHE file) or
# the MIT license (the LICENSE-MIT file) at your option. This file may not be
# copied, modified, or distributed except according to those terms.

set -x
set -e

if [ "$RUST_NIGHTLY" != "1" ]; then
  exit 0
fi

export RUST_TEST_THREADS=1

cargo build
for feature in '' large-align; do
  RUST_BACKTRACE=1 cargo test --features "$feature"
done
