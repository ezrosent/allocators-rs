#!/bin/bash

set -x
set -e

cargo build --verbose --all
RUST_BACKTRACE=1 cargo test --verbose --all -- --ignored
