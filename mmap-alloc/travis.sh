#!/bin/bash

set -x
set -e

cargo build --verbose --all
RUST_BACKTRACE=1 cargo test --verbose --all -- --ignored
for feature in test-no-std; do
  RUST_BACKTRACE=1 cargo test --verbose --all --features "$feature" -- --ignored
done
