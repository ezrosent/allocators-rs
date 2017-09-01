#!/bin/bash

# Copyright 2017 the authors. See the 'Copyright and license' section of the
# README.md file at the top-level directory of this repository.
#
# Licensed under the Apache License, Version 2.0 (the LICENSE file). This file
# may not be copied, modified, or distributed except according to those terms.

set -x
set -e

travis-cargo --only nightly build
RUST_BACKTRACE=1 travis-cargo --only nightly test
for feature in nightly; do
  RUST_BACKTRACE=1 travis-cargo --only nightly test -- --features "$feature"
done
