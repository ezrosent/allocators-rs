<!-- Copyright 2017 the authors. See the 'Copyright and license' section of the
README.md file at the top-level directory of this repository.

Licensed under the Apache License, Version 2.0 (the LICENSE-APACHE file) or
the MIT license (the LICENSE-MIT file) at your option. This file may not be
copied, modified, or distributed except according to those terms. -->

# elfmalloc Performance Evaluation

This document describes the performance of `elfmalloc` in existing benchmarks
that have been used to evaluate `malloc` performance in C/C++. Using the
`LD_PRELOAD` mechanism on Linux, we can get something of an apples-to-apples
measure of how our work stacks up.

This document is a work in progress. We are currently working on providing
results for more `malloc` implementations in more workloads. We are also
exploring various optimizations for the allocator. Various tuning parameters
improve `elfmalloc` performance substantially in some of these benchmarks,
sometimes at the cost of performance elsewhere. We are also still investigating
the best balance of these parameters. We are also still investigating the
different benchmark settings in use to ensure that all settings are fair wrt
different allocators' thread-local cache sizes.

## Two variants of `elfmalloc`

In addition to a standard version (called simply `elfmalloc`), we provide the
variant `elfmalloc-l`. The `-l` refers to the use of the `LocalCache` front-end
for the allocator, as opposed to the `MagazineCache` front-end used in the
default configuration. While `elfmalloc` is almost always superior in terms of
both throughput and memory usage, `elfmalloc-l` is useful for performance
evaluation as it provides a "no-cache" baseline for allocator performance.

## Benchmarks

We use a subset of the benchmarks used in the `scalloc`
[paper](https://arxiv.org/pdf/1503.09006.pdf). These benchmarks
come from several teams of developers that have worked on allocator
performance over the years. We describe them briefly here, but we
encourage anyone curious to examine Section 7 of that paper.

  * *Threadtest*: This benchmark allocates and deallocates a number of
    small objects in rounds each round with several allocations, causing
    thread-local caches to be overrun. While this is performed with
    multiple threads, we call this workload "thread-local" as all pointers
    are freed in the same thread in which they were
    allocated. Threadtest was developed to assess allocator performance
    during [Hoard's](http://www.cs.utexas.edu/users/mckinley/papers/asplos-2000.pdf)
    development.

  * *Shbench*: Similar to *Threadtest* but with varying object sizes and
    varying object lifetimes. Our understanding is that this benchmark is due to
    [Larson et al](https://pdfs.semanticscholar.org/e41a/d0406628edf82712d16cf4c6d7e486f26f9f.pdf).

  * *rpmalloc-benchmark*: This benchmark performs a (somewhat unrealistic)
    workload of randomly-distributed allocation sizes. This benchmark is
    included because it can be configured to provide a succinct
    producer-consumer workload: one where objects are allocated and freed in
    different threads. This is the configuration we use in the measurements
    below.

  <!-- 

  NOTE: we no longer report results from this workload, as I do not believe the
  original numbers were reported correctly. We will look into how scalloc
  reports results from this benchmark and potentially re-add them in the future.

  * *ACDC Producer-Consumer Workload*: This benchmark involves each
    thread sending a portion of its allocated objects to other threads
    participating in the benchmark. Those threads (not the allocating
    thread) are the ones that ultimately free those objects. The [ACDC
    framework](https://github.com/cksystemsgroup/ACDC) was developed by
    the scalloc authors.
  -->

The data here were gathered using a version of the [`scalloc`
artifact](https://github.com/cksystemsgroup/scalloc-artifact).  The only
modifications to the artifact were tweaks of object sizes (for the "large
object" variants) and iteration counts to reduce variance and test robustness
for larger workloads. The rpmalloc benchmark is not included in the artifact, so
we adapted it to output similar results.

These benchmarks were conducted on a 16-core 32-thread workstation with
2 Xeon E5-2620v4 CPUs on the WSL. We benchmark these workloads at 1,
2, 4, 8, 16, 24, and 32 threads. For the (1-8) thread configurations,
all use physical cores. For 16 threads, all threads are scheduled on a
single socket using all hardware threads. The 24-thread configuration
uses some subset of the available hardware threads, this time crossing
a NUMA domain.  Finally, the 32-thread benchmark uses all available
hardware threads across both sockets.

### Other Allocators Measured

We include two representative allocators to benchmark against. We plan
to add more in the future (e.g. `scalloc`, which we are having trouble
getting to run on WSL due to some `mmap`-related issues).

  * `jemalloc`: A mature, efficient `malloc` used as the default in the
    Rust ecosystem and used in production at Facebook. We pulled a dev
    version of this allocator in early August 2017. Many of its numbers
    appear improved over the earlier version benchmarked in the `scalloc`
    paper. [link](https://github.com/jemalloc/jemalloc)

  * `llalloc`: An efficient, proprietary `malloc` from Lockless Inc.
    [link](https://locklessinc.com/)

  * `ptmalloc2`: The default allocator on Linux. Throughput numbers are
    expressed as a multiple of the numbers for this allocator. These
    numbers represent performance for ptmalloc version 2.19; the default
    present from the version of glibc in use.

  * `rpmalloc`: [link](https://github.com/rampantpixels/rpmalloc) for more
     information.

## Measurements

We provide measurements of both memory consumption and throughput for the 3
workloads described above. A common theme here is that `elfmalloc` provides
consistently high throughput, sometimes at the cost of increased memory usage.
By the same token, if you see an allocator failing to scale up it is worth
looking at heap growth for the same workload. Some allocators like `rpmalloc`
return memory to the OS more aggressively than `elfmalloc`, effectively trading
off improved memory efficiency for reduced throughput. We are still working on
tuning `elfmalloc` to be less profligate with memory under certain
circumstances.

In order to make the graphs at all readable, we express throughput in terms of a
multiple over the performance of `ptmalloc2`, which is consistently the slowest
allocator. For `threadtest` and `shbench`, we provide numbers for both small (64
bytes or smaller) and medium-sized (a few KB) objects. For throughput, more is
better; for memory consumption less is better.

### Threadtest

Threadtest shows `elfmalloc` having the highest throughput (except for 32
threads, where it is a bit behind `jemalloc`), at the cost of a noticeable
increase in memory usage.

*Threadtest Throughput (Small Objects)*

![Threadtest Throughput](elfmalloc-data/threadtest-small-tp.png?raw=true)

*Threadtest Memory Consumption (Small Objects)*

![Threadtest Memory](elfmalloc-data/threadtest-small-mem.png?raw=true)

For larger objects, `elfmalloc` comes out on top in terms of throughput for 32
threads. Unlike the case of smaller objects, `elfmalloc` has memory usage
roughly in line with that of `jemalloc`. One interesting point here is the
memory usage of `elfmalloc-l`. Here we see the counter-intuitive result that
increased thread-local cache sizes can lead to *lower* memory consumption. We
suspect that this is because `threadtest` includes long stretches of allocation
followed by relatively limited deallocation, limiting the re-use of allocated
pages.

*Threadtest Throughput (Medium Objects)*

![Threadtest Throughput](elfmalloc-data/threadtest-large-tp.png?raw=true)

*Threadtest Memory Consumption (Medium Objects)*

![Threadtest Memory](elfmalloc-data/threadtest-large-mem.png?raw=true)

### Shbench

For `shbench`, `elfmalloc` out-performs all competition in terms of both memory
and throughput for higher core-counts, though it lags behind `llalloc` early
on.. `elfmalloc-l` performs similarly well, though slightly worse on average for
smaller objects, and slightly better for larger objects.  `elfmalloc` also
provides very good memory performance across the board, sometimes better than
`jemalloc`.

Also of note is the memory consumption of `ptmalloc2`, `llalloc` and `rpmalloc`:
something about the varying lifetimes of objects seems to trip these allocators
up here, as they use over an order of magnitude more memory than `jemalloc` and
`elfmalloc`.

*Shbench Throughput (Small Objects)*

![Shbench Throughput](elfmalloc-data/shbench-small-tp.png?raw=true)

*Shbench Memory Consumption (Small Objects)*

![Shbench Memomry](elfmalloc-data/shbench-small-mem.png?raw=true)

We see a similar improvement for shebench on larger objects.

*Shbench Throughput (Medium Objects)*

![Shbench Large Object Throughput](elfmalloc-data/shbench-large-tp.png?raw=true)

*Shbench Memory Consumption (Small Objects)*

![Shbench Large Object Memory](elfmalloc-data/shbench-large-mem.png?raw=true)

### Producer-Consumer

Like threadtest, `elfmalloc` has the highest throughput in this workload at the
cost of increased memory consumption.

*Producer-Consumer Throughput*

![Producer-Consumer Throughput](elfmalloc-data/prod-cons-tp.png?raw=true)

*Producer-Consumer Memory Consumption*

![Producer-Consumer Memory](elfmalloc-data/prod-cons-mem.png?raw=true)
