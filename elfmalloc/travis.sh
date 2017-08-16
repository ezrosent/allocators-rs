#!/bin/bash

set -x
set -e

cargo build --verbose --all
RUST_BACKTRACE=1 cargo test --verbose --all -- --ignored
for feature in prime_schedules huge_segments no_lazy_region nightly; do
  RUST_BACKTRACE=1 cargo test --verbose --all --features "$feature" -- --ignored
done
