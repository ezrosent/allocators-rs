#!/bin/bash

set -x
set -e

cargo build --verbose --all
# Use opt-level=3 because the corruption tests take a long time, and the
# optimized build saves more time in test execution than it adds in compilation
# time.
RUSTFLAGS='-C opt-level=3' RUST_BACKTRACE=1 cargo test --verbose --all -- --ignored
# TODO: Once no-std and no-os work, add those features.
for feature in build-ignored-tests use-stdlib-hashmap no-coloring hashmap-no-resize hashmap-no-coalesce; do
  RUSTFLAGS='-C opt-level=3' RUST_BACKTRACE=1 cargo test --verbose --all --features "$feature" -- --ignored
done
