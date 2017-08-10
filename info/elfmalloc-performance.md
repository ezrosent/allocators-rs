# elfmalloc Performance Evaluation

This document describes the performance of `elfmalloc` in existing
benchmarks that have been used to evaluate `malloc` performance in
C/C++. Using the `LD_PRELOAD` mechanism on Linux, we can get something
of an apples-to-apples measure of how our work stacks up.

Currently, we povide comparable or better performance to the state of
the art.

This document is a work in progress. We are currently working on
providing results for more `malloc` implementations in more
workloads. We are also exploring various optimizations for the
allocator.

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
    varying object lifetimes. We also provide numbers for larger (1-8K)
    object sizes in addition to the smaller object sizes used by
    *Threadtest*.

  * *ACDC Producer-Consumer Workload*: This benchmark involves each
    thread scattering a portion of its allocations among all other threads
    participating in the benchmark. Threads that receive these allocations
    are the ones that free the objects themselves.

The data here were gathered using a version of the
[`scalloc` artifact](https://github.com/cksystemsgroup/scalloc-artifact).
The only modifications to the artifact were ones that allowed it to run
on our testing setup.

These benchmarks were conducted on a 16-core 32-thread workstation with
2 Xeon E5-2620v4 CPUs on the Windows Subsystem for Linux (WSL).

### Competing Allocators

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

## Measurements

We provide measurements of both memory consumption throughput for the 3
workloads described above. In general, we provide comparable
(though sometimes slightly worse) throughput to `llalloc` while using
substantially less memory. Compared with `jemalloc` we are consistently
more efficient, though we have a higher memory overhead.

We have had some trouble getting the rightmost points on some of these
graphs to show differences in performance. As a result, we report those
numbers directly as well as with a graph.

### Threadtest

![Threadtest Throughput](elfmalloc-data/threadtest-tp.png?raw=true)

For throughput, the final numbers for `elfmalloc`, `jemalloc` and
`llalloc` are 4.2, 4.9, and 3.1, respectively.

![Threadtest Memory](elfmalloc-data/threadtest-mem.png?raw=true)


### Shbench

For `shbench` we see the same trend: the difference being that
`jemalloc` and `elfmalloc` both have much lower single-threaded
performance than `llalloc`, though they use an order of magnitude less
memory.

![Shbench Throughput](elfmalloc-data/shbench-tp.png?raw=true)
![Shbench Memomry](elfmalloc-data/shbench-mem.png?raw=true)

For throughput, the final numbers for `elfmalloc`, `jemalloc` and
`llalloc` are 0.74, 0.96, and 0.67, respectively.

Now we move onto the larger object sizes:

![Shbench Large Object Throughput](elfmalloc-data/shbench-large-tp.png?raw=true)
![Shbench Large Object Memomry](elfmalloc-data/shbench-large-mem.png?raw=true)

We just barely pull ahead here: For throughput, the final numbers
for `elfmalloc`, `jemalloc` and `llalloc` are 1.6, 1.7, and 2.7,
respectively.

### Producer-Consumer

![Producer-Consumer Throughput](elfmalloc-data/prodcons-tp.png?raw=true)
![Producer-Consumer Memomry](elfmalloc-data/prodcons-mem.png?raw=true)
