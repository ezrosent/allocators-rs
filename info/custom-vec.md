# Fun with the Rust `Alloc` trait

This post goes through the new Rust allocation infrastructure, including an
example of a `Vec` type with a custom allocator.

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
  // several other methods in the full trait.
}
```

Where
[`Layout`](https://doc.rust-lang.org/nightly/alloc/allocator/struct.Layout.html)
is a type that encapsulates size and alignment information, and
[`AllocErr`](https://doc.rust-lang.org/nightly/alloc/allocator/enum.AllocErr.html)
specifies a few common errors that allocators can exhibit (e.g. OOM). The
biggest difference between this interface and the more traditional one is the
fact that the caller provides size information during deallocation.

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

* We no longer rely on mapping a huge segment of memory at startup: when a
  thread needs fresh memory from the OS, it just calls `mmap` for a few more
  pages.

* We only use the slab subsystem for relatively small objects. Medium-sized
  objects are now allocated directly from `PageAlloc` structures. This cuts down
  on metadata management and fragmentation for larger objects.

<!-- TODO(ezrosent): fill in this link once pushed to master -->
For more information on this design, check out the documentation.

## Using a Custom `Vec`

My main goal here was to try this out in some fairly idiomatic Rust code; the
path of least resistance was definitely `Vec`. Not only is `Vec` one of the most
widely used Rust constructs that relies on heap allocation, but a lot of the
work is already done for me. Much of the lower-level memory management that
`Vec` performs is factored out into
[`RawVec`](https://doc.rust-lang.org/nightly/alloc/raw_vec/struct.RawVec.html),
and `RawVec` is now parametric on an `Alloc` instance.

Given that, implementing a data-structure with essentially the same interface as
`Vec` was as simple as filling in some basic boilerplate. The
[smallvec](https://github.com/servo/rust-smallvec) project was an excellent
guide when doing this. This custom allocator-parametric `Vec` is currently
called `AVec`.

## Performance

**Warning**: These numbers appear to stress the `mmap` implementation in
[Windows Subsystem for Linux](https://blogs.msdn.microsoft.com/commandline/learn-about-bash-on-windows-subsystem-for-linux/)
(WSL)'s for benchmarks with larger objects. As such, the numbers may not easily
transfer to other systems.  We benchmarked 3 separate `Vec` types:

1. Rust's native `Vec` (suffix `_vec`)
2. `AVec` parameterized on `Heap`  (suffix `_avec_heap`)
3. `AVec` parameterized on the rust-specific `elfmalloc` (suffix `_avec_elf`)

Options 1 and 2 both allocate memory using `jemalloc`. They should ideally be
the same, but both are included in order to flag any cases in which the
implementations of `AVec` and `Vec` might diverge. We currently benchmark 3
workloads:

**PSA**: If you are ever doing operations like this in a tight loop and
performance is a serious concern, you are probably better off calling `reserve`
or initializing using `with_capacity` to pre-allocate all the needed space ahead
of time. These are microbenchmarks, and as a result they do have caveats with
regard to how they translate to impacts on real workloads.

*Workload 1: Small Allocations* This involves creating a vector and pushing 500
`usize`s onto it. We start all vectors at size 0, so this should involve a small
number of reallocations from doubling operations. We include results with both 1
thread and 32 threads. Median absolute deviation is reported across threads, not
iterations (that will be fixed soon).

The multithreaded numbers represent the same workload run across several
threads, with each thread's run reported as a separate sample for the mean and
variation statistics. One would expect an algorithm that scales perfectly to
have numbers matching the single-threaded performance of the workload.

```
benchmark-n01 bench_push_avec_elf                      1.992 us     per iteration
benchmark-n32 bench_push_avec_elf                      4.432 us     per iteration (+/- 16.690 ns)
benchmark-n01 bench_push_vec                           2.091 us     per iteration
benchmark-n32 bench_push_vec                           4.876 us     per iteration (+/- 20.869 ns)
benchmark-n01 bench_push_avec_heap                     2.088 us     per iteration
benchmark-n32 bench_push_avec_heap                     4.961 us     per iteration (+/- 15.524 ns)
```

Here we can see that these are about the same. This makes sense: all of these
allocations are going to be hitting thread-local caching layers and those
data-structures seem to be fairly well-optimized in both cases. There is also
fairly little room for improvement here: 2 microseconds for 5000 push operations
on a processor that turbos to 2.3Ghz is pretty good.

*Workload 2: Medium Allocations*: This workload is very similar to the first,
except that it starts the benchmark with a vector with 16K elements and pushes
500,000 more `usize`s onto the vector. This workload is repeated 50 times.

```
benchmark-n01 bench_push_medium_avec_elf                2.739 ms     per iteration
benchmark-n32 bench_push_medium_avec_elf                31.042 ms    per iteration (+/- 3.975 ms)
benchmark-n01 bench_push_medium_vec                     8.428 ms     per iteration
benchmark-n32 bench_push_medium_vec                     263.647 ms   per iteration (+/- 2.395 ms)
benchmark-n01 bench_push_medium_avec_heap               7.570 ms     per iteration
benchmark-n32 bench_push_medium_avec_heap               259.291 ms   per iteration (+/- 1.854 ms)
```

These numbers are quite good! Too good, I suspect. While part of this speedup
may be due to the scalable `Bagpipe` data-structure we use for medium objects,
it is possible that WSL is getting in `jemalloc`'s way here. My current theory
is that `jemalloc` is more aggressive with reclaiming memory than we are in this
scenario; that means it has to call `mmap` and `munmap` a lot more than
`elfmalloc`. This is exacerbated by the fact that these system calls appear to
be something of a scaling bottleneck on WSL, though that is just an educated
guess by looking at per-core load running certain workloads.


*Workload 3: Large Allocations*: This workload is essentially the same, but
instead of pushing `usize`s onto the vectors we push `[usize; 1024]`s. The
benchmark pushes 1000 of these and repeats the workload 50 times.

```
benchmark-n01 bench_push_large_avec_elf               10.529 ms    per iteration
benchmark-n32 bench_push_large_avec_elf               372.935 ms   per iteration (+/- 1.832 ms)
benchmark-n01 bench_push_large_vec                    14.107 ms    per iteration
benchmark-n32 bench_push_large_vec                    556.480 ms   per iteration (+/- 3.350 ms)
benchmark-n01 bench_push_large_avec_heap              16.099 ms    per iteration
benchmark-n32 bench_push_large_avec_heap              550.472 ms   per iteration (+/- 3.078 ms)
```

Here, `elfmalloc` again performs better than the default heap allocator. I
suspect that this is due to our use of the `BagPipe` data-structure to cache
these objects. `jemalloc` may simply fall back on `mmap` for allocations this
large. `elfmalloc` does this as well, but it has a higher threshold here. The
improvement at this end demonstrates the benefit of being able to easily
configure one's allocator for a particular workload if required. 



