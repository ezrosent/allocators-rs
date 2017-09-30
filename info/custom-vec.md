<!-- Copyright 2017 the authors. See the 'Copyright and license' section of the
README.md file at the top-level directory of this repository.

Licensed under the Apache License, Version 2.0 (the LICENSE-APACHE file) or
the MIT license (the LICENSE-MIT file) at your option. This file may not be
copied, modified, or distributed except according to those terms. -->
# Fun with the Rust `Alloc` trait

This post gives a brief tour of Rust's new allocation interface, and some
experiments with a custom `Vec` implementation.

## Allocation in Rust

Rust has recently made progress in defining an interface for general memory
allocation. This interface is different from the standard `malloc`/`free`
interface from C. Here is a shorter version of the [full
interface](https://doc.rust-lang.org/nightly/alloc/allocator/trait.Alloc.html):

```rust
pub unsafe trait Alloc {
  unsafe fn alloc(&mut self, layout: Layout) -> Result<*mut u8, AllocErr>;
  unsafe fn dealloc(&mut self, ptr: *mut u8, layout: Layout);
  // alloc and dealloc are sufficient to implement this trait, but there are
  // several other methods in the full trait that have default implementations.
}
```

Where
[`Layout`](https://doc.rust-lang.org/nightly/alloc/allocator/struct.Layout.html)
is a type that encapsulates size and alignment information, and
[`AllocErr`](https://doc.rust-lang.org/nightly/alloc/allocator/enum.AllocErr.html)
specifies a few common errors that allocators can exhibit (e.g. OOM). The
biggest difference between this interface and the more traditional one is that
allocation requests take an explicit alignment, and size and alignment are
provided on deallocation and reallocation.

## Implications for `elfmalloc`

`elfmalloc` is
[currently designed](https://github.com/ezrosent/allocators-rs/blob/master/info/elfmalloc.md)
to support the C interface. As a result, it does extra work to store size
information on the heap so that it can be looked up during a call to `free`.
While this means that the current implementation has enough information to
implement the `Alloc` trait, I have started thinking about ways that the overall
design could be improved or simplified if it *only* had to implement that trait. 

The end result is a similar design that differs from the original in two chief
ways:

* We do not require complex metadata management to differentiate allocation
  subsystems (see "Determining Allocation Source..." at the bottom of the
  [elfmalloc doc](https://github.com/ezrosent/allocators-rs/blob/master/info/elfmalloc.md))

* We only use the slab subsystem for relatively small
  objects. Medium-sized objects are now allocated directly from
  `PageAlloc`-like structures. This cuts down on metadata management and
  fragmentation for larger objects.

For more information on this design, check out [the
code](https://github.com/ezrosent/allocators-rs/blob/master/elfmalloc/src/rust_alloc.rs).

## Using a Custom `Vec`

My main goal here was to try this out in some fairly idiomatic Rust code; the
path of least resistance was definitely `Vec`. Not only is `Vec` one of the most
widely used Rust constructs that relies on heap allocation, but a lot of the
work is already done for me. Much of the lower-level memory management that
`Vec` performs is factored out into
[`RawVec`](https://doc.rust-lang.org/nightly/alloc/raw_vec/struct.RawVec.html),
and `RawVec` is now parametric on an `Alloc` instance.

Given that, implementing a data-structure with essentially the same interface as
`Vec` was as simple as:

```rust
pub struct AVec<T, A: Alloc> {
    buf: RawVec<T, A>,
    len: usize,
}

impl<T, A: Alloc> AVec<T, A> {
    fn push(&mut self, val: T) {
        if self.len == self.buf.cap() {
            self.buf.double();
        }
        unsafe {
            ptr::write(self.buf.ptr().offset(self.len as isize), val);
        }
        self.len += 1;
    }

    unsafe fn to_slice(&self) -> *mut [T] {
        ::std::slice::from_raw_parts_mut(self.buf.ptr(), self.len)
    }
}
```

Along with some boilerplate trait implementations. The
[smallvec](https://github.com/servo/rust-smallvec) project was an excellent
guide when doing this.

## Performance

> **Warning**: These numbers appear to stress the `mmap` implementation in
> [Windows Subsystem for Linux](https://blogs.msdn.microsoft.com/commandline/learn-about-bash-on-windows-subsystem-for-linux/)
> (WSL) for benchmarks with larger objects. As such, the numbers may not easily
> transfer to other systems. 

This performance evaluation includes 3 separate `Vec` types:

1. Rust's native `Vec` (suffix `_vec`)
2. `AVec` instantiated with `Heap`  (suffix `_avec_heap`)
3. `AVec` instantiated with the rust-specific `elfmalloc` (suffix `_avec_elf`)

Options 1 and 2 both allocate memory using `jemalloc`. They should ideally be
the same, but both are included in order to flag any cases in which the
implementations of `AVec` and `Vec` might diverge. There are currently
measurements for 3 workloads:

> **PSA**: If you are ever doing operations like this in a tight loop and
> performance is a serious concern, you are probably better off calling `reserve`
> or initializing using `with_capacity` to pre-allocate all the needed space ahead
> of time. These are microbenchmarks, and as a result they do have caveats with
> regard to how they translate to impacts on real workloads.

*Workload 1: Small Allocations* This involves creating a vector and pushing 500
`usize`s onto it. We start all vectors at size 0, so this should involve a small
number of reallocations from doubling operations. We include results with both 1
thread and 32 threads. For this workload only, we have an "inner loop" where the
500-push workload is repeated 8 times; this is to reduce the overhead caused by
having too many samples.

The multithreaded numbers represent the same workload run across several
threads, with each thread's run reported as a separate sample for the mean and
variation statistics. One would expect an algorithm that scales perfectly to
have numbers matching the single-threaded performance of the workload.

```
benchmark-n01 bench_push_avec_elf                      15.883 μs    per iteration (+/- 10.59%)
benchmark-n32 bench_push_avec_elf                      38.035 μs    per iteration (+/- 4.01%)
benchmark-n01 bench_push_vec                           17.954 μs    per iteration (+/- 9.27%)
benchmark-n32 bench_push_vec                           37.127 μs    per iteration (+/- 4.12%)
benchmark-n01 bench_push_avec_heap                     17.865 μs    per iteration (+/- 9.27%)
benchmark-n32 bench_push_avec_heap                     37.468 μs    per iteration (+/- 4.01%)
```

Here we can see that these are about the same, perhaps with a slight edge to
`elfmalloc` in a single-threaded setting. This makes sense: all of these
allocations are going to be hitting thread-local caching layers and the
corresponding data-structures seem to be fairly well-optimized in both cases.
There is also fairly little room for improvement here: 16 microseconds for 4000
push operations on a processor that turbos to 2.3Ghz is pretty good.

*Workload 2: Medium Allocations*: This workload is very similar to the first,
except that it starts the benchmark with a vector with 16K elements and pushes
500,000 more `usize`s onto the vector. This workload is repeated 50 times.

```
benchmark-n01 bench_push_medium_avec_elf               2.565 ms     per iteration (+/- 0.83%)
benchmark-n32 bench_push_medium_avec_elf               23.415 ms    per iteration (+/- 38.84%)
benchmark-n01 bench_push_medium_vec                    8.239 ms     per iteration (+/- 2.45%)
benchmark-n32 bench_push_medium_vec                    271.837 ms   per iteration (+/- 4.97%)
benchmark-n01 bench_push_medium_avec_heap              7.901 ms     per iteration (+/- 2.94%)
benchmark-n32 bench_push_medium_avec_heap              272.775 ms   per iteration (+/- 4.85%)
```

These numbers are quite good! Too good, I suspect; and what is going on with the
high variance for 32 threads? While part of this speedup may be due to the
scalable
[`BagPipe`](https://github.com/ezrosent/allocators-rs/blob/master/info/bagpipes.md)
data-structure we use for medium objects, it is possible that WSL is getting in
`jemalloc`'s way here. My current theory is that `jemalloc` is more aggressive
with reclaiming memory than we are in this scenario; that means it has to call
`mmap` and `munmap` a lot more than `elfmalloc`. This is exacerbated by the fact
that these system calls appear to be something of a scaling bottleneck on WSL,
though that is just an educated guess by looking at per-core load running
certain workloads.


*Workload 3: Large Allocations*: This workload is essentially the same, but
instead of pushing `usize`s onto the vectors we push `[usize; 1024]`s. The
benchmark pushes 1000 of these and repeats the workload 50 times.

```
benchmark-n01 bench_push_large_avec_elf                10.407 ms    per iteration (+/- 4.07%)
benchmark-n32 bench_push_large_avec_elf                368.045 ms   per iteration (+/- 5.18%)
benchmark-n01 bench_push_large_vec                     14.317 ms    per iteration (+/- 2.80%)
benchmark-n32 bench_push_large_vec                     560.585 ms   per iteration (+/- 3.27%)
benchmark-n01 bench_push_large_avec_heap               15.015 ms    per iteration (+/- 3.07%)
benchmark-n32 bench_push_large_avec_heap               568.226 ms   per iteration (+/- 4.10%)
```

Here, `elfmalloc` again performs better than the default heap allocator. I
suspect that this is due to our use of the `BagPipe` data-structure to cache
these objects. `jemalloc` may simply fall back on `mmap` for allocations this
large. `elfmalloc` does this as well, but it has a higher threshold here. The
improvement at this end demonstrates the benefit of being able to easily
configure one's allocator for a particular workload if required. 

### Running These Benchmarks

In the `elfmalloc` crate, build with

```
cargo build --release --features=local_cache,use_default_allocator
```

And then run `target/release/bench_vec`. It will automatically scale to the
number of available hardware threads on the current machine. It is highly
unlikely that these benchmarks will run on anything but Linux.

## Memory Consumption?

I currently do not have a good solution for tracking fine-grained memory
consumption in pure-Rust benchmarks like this. I have an idea of how to
replicate the techniques used in the more mature [`malloc`
benchmarks](https://github.com/ezrosent/allocators-rs/blob/master/info/elfmalloc-performance.md)
we have access to, but getting everything working well will take a good deal of
time.

As for rough estimates, I did keep a close eye on task manager throughout the
benchmark execution. While it is a very rough metric; I believe `elfmalloc` and
`jemalloc` used comparable memory (within, say, a factor of 2), with `jemalloc`
often using less memory.
