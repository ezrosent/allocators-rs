# `bsalloc`: a global, `no-std` dynamic memory allocator.

This crate implements a very simple global allocator. This is used to
service heap allocations for dependencies of the dynamic allocators in
the rest of the repo. In other words, this is used to **b**oot**s**trap
the more efficient allocators in this repo without creating a needles
dependency on another malloc implementation.

## Structure

The allocator is a thin wrapper around `mmap`. Instead of simply calling
`mmap` and `munmap` for dynamic allocations directly (this did slow
things down in some microbenchmarks), we have two "size classes",
each with a fixed-size global cache. These caches have a thread-safe
interface that allows a thread to efficiently re-use freed by it or
another thread. Naturally, allocations that are larger than the largest
size class fall back on `mmap` directly.
