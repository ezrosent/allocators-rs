<!-- Copyright 2017 the authors. See the 'Copyright and license' section of the README.md file at the top-level directory of this repository.

Licensed under the Apache License, Version 2.0 (the LICENSE file). This file may not be copied, modified, or distributed except according to those terms. -->

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
for the allocator, as opposed to the `MagazineCache` frontend used in the
default configuration. While `elfmalloc` is almost always superior in terms of
both throughput and memory usage, `elfmalloc-l` provides better performance in
the producer-consumer benchmark.

While we are still working on improving the default to have the advantages of
both frontends, we report the performance numbers of both allocators for the
time being.

## Benchmarks

We use a subset of the benchmarks used in the `scalloc`
[paper](https://arxiv.org/pdf/1503.09006.pdf). We describe them briefly
here, but we encourage anyone curious to examine Section 7 of that paper.

  * *Threadtest*: This benchmark allocates and deallocates number of
    small objects in rounds each round with several allocations, causing
    thread-local caches to be overrun. While this is performed with
    multiple threads, we call this workload "thread-local" all pointers
    are freed in the same thread in which they were allocated.

  * *Shbench*: Similar to *Threadtest* but with varying object sizes and
    varying object lifetimes.

  * *ACDC Producer-Consumer Workload*: This benchmark involves each
    thread scattering a portion of its allocations among all other threads
    participating in the benchmark. Threads that receive these allocations
    are the ones that free the objects themselves.

The data here were gathered using a version of the [`scalloc`
artifact](https://github.com/cksystemsgroup/scalloc-artifact).  The only
modifications to the artifact were ones that allowed it to run on our testing
setup.

These benchmarks were conducted on a 16-core 32-thread workstation with 2 Xeon
E5-2620v4 CPUs on the Windows Subsystem for Linux (WSL). We benchmark these
workloads at 1, 2, 4, 8, 16, 24, and 32 threads. For the (1-8) thread
configurations, all use physical cores. For 16 threads, all threads are
scheduled on a single socket using all hardware threads. The 24-thread
configuration uses some subset of the available hardware threads, this time
crossing a NUMA domain.  Finally, the 32-thread benchmark uses all available
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
    expressed as a multiple of the numbers for this allocator.

## Measurements

We provide measurements of both memory consumption throughput for the 3
workloads described above. A common theme among these results is that `llalloc`
provides substantially higher throughput compared to `jemalloc`, at the cost of
much higher memory consumption. By contrast, `elfmalloc` often provides better
performance than `llalloc` while using a similar amount of memory to `jemalloc`.

In order to make the graphs at all readable, we express throughput in terms of a
multiple over the performance of `ptmalloc2`, which is consistently the slowest
allocator. For `threadtest` and `shbench`, we provide numbers for both small (64
bytes or smaller) and medium-sized (a few KB) objects.

### Threadtest

Threadtest shows `elfmalloc` almost splitting the difference between `jemalloc`
and `llalloc`: both in terms of memory usage and throughput. `elfmalloc-l` has
dissapointing memory performance that we are still looking into.

*Threadtest Throughput (Small Objects)*

![Threadtest Throughput](elfmalloc-data/threadtest-small-tp.png?raw=true)

*Threadtest Memory Consumption (Small Objects)*

![Threadtest Memory](elfmalloc-data/threadtest-small-mem.png?raw=true)

For larger objects, `elfmalloc` gets closer in terms of throughput to `llalloc`
while exceeding the memory efficiency of `jemalloc` at higher thread-counts.
Both object sizes show an odd spike of memory usage at 4 and 8 threads: we are
still looking into why this occurs.

*Threadtest Throughput (Medium Objects)*

![Threadtest Throughput](elfmalloc-data/threadtest-large-tp.png?raw=true)

*Threadtest Memory Consumption (Medium Objects)*

![Threadtest Memory](elfmalloc-data/threadtest-large-mem.png?raw=true)

### Shbench

For `shbench`, `elfmalloc` out-performs all competition in terms of both memory
and throughput for higher core-counts, though it lags behind `llalloc` early
on.. `elfmalloc-l` performs similarly well, though slightly worse on average.
It provides very good memory performance across the board, sometimes better
than `jemalloc`.

Also of note is the memory consumption of `ptmalloc2` and `llalloc`: something
about the varying lifetimes of objects seems to trip these allocators up here,
as they use over an order of magnitude more memory than `jemalloc` and
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

As mentioned above, the producer-consumer benchmark shows `elfmalloc-l`
outperforming the other allocators in terms of throughput, while using memory
at a level between those of `llalloc` and `jemalloc`. `elfmalloc` performs
similarly, though it often falls behind `jemalloc` in terms of throughput.

*Producer-Consumer Throughput*

![Producer-Consumer Throughput](elfmalloc-data/prod-cons-tp.png?raw=true)

*Producer-Consumer Memory Consumption*

![Producer-Consumer Memory](elfmalloc-data/prod-cons-mem.png?raw=true)
