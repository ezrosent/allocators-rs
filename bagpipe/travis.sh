#!/bin/bash

set -x
set -e

cargo build --verbose --all
RUST_BACKTRACE=1 cargo test --verbose --all -- --ignored
for feature in check_empty_yq prime_schedules staggered_indexes huge_segments; do
  RUST_BACKTRACE=1 cargo test --verbose --all --features "$feature" -- --ignored
done
