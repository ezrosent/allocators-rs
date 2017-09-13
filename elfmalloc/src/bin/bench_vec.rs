// Copyright 2017 the authors. See the 'Copyright and license' section of the
// README.md file at the top-level directory of this repository.
//
// Licensed under the Apache License, Version 2.0 (the LICENSE-APACHE file) or
// the MIT license (the LICENSE-MIT file) at your option. This file may not be
// copied, modified, or distributed except according to those terms.

#![feature(alloc)]
#![feature(log_syntax)]
#![feature(allocator_api)]
#![feature(test)]
extern crate alloc;
extern crate elfmalloc;
extern crate num_cpus;
extern crate smallvec;
extern crate test;
use test::stats::Stats;
use elfmalloc::rust_alloc::SharedAlloc;
use elfmalloc::vec_alloc::AVec;
use alloc::heap::Heap;
use smallvec::VecLike;

use std::thread;
use std::time;
use std::sync::{Arc, Barrier};

/// A basic resumable timer type used for benchmark time measurements.
struct Timer {
    start: time::Instant,
    elapsed: time::Duration,
}

impl Default for Timer {
    fn default() -> Timer {
        Timer {
            start: time::Instant::now(),
            elapsed: time::Duration::default(),
        }
    }
}

impl Timer {
    fn new() -> Self {
        Timer::default()
    }
    fn reset(&mut self) {
        *self = Timer::new();
    }
    fn stop(&mut self) {
        let cur_elapsed = self.start.elapsed();
        self.elapsed += cur_elapsed;
    }
    fn resume(&mut self) {
        self.start = time::Instant::now();
    }
    fn elapsed_ns(&self) -> u64 {
        let dur = self.start.elapsed() + self.elapsed;
        dur.as_secs() * 1_000_000_000 + u64::from(dur.subsec_nanos())
    }
}


/// Run a workload given set parameters in parallel, timing the execution of each thread.
///
/// This function returns an `f64` because `[f64]` has some pre-written statistics computationss
/// from the `test` crate that we can reuse.
fn run_parallel_bench<A: Clone + Send + 'static>(
    params: A,
    threads: usize,
    work: fn(A, &mut Timer) -> Vec<f64>,
) -> Vec<f64> {
    let b = Arc::new(Barrier::new(threads + 1));
    let threads: Vec<_> = (0..threads)
        .map(|_| {
            let p = params.clone();
            let b = b.clone();
            thread::spawn(move || {
                b.wait();
                let mut t = Timer::new();
                work(p, &mut t)
            })
        })
        .collect();
    b.wait();
    let mut res = Vec::new();
    for t in threads.into_iter().map(|j| {
        j.join().expect("threads should exit successfully")
    })
    {
        res.extend(t)
    }
    res
}

fn format_dur(mut nsecs: f64) -> String {
    if nsecs >= 1_000_000_000f64 {
        nsecs /= 1_000_000_000f64;
        format!("{:.03} s", nsecs)
    } else if nsecs >= 1_000_000f64 {
        nsecs /= 1_000_000f64;
        format!("{:.03} ms", nsecs)
    } else if nsecs >= 1_000f64 {
        nsecs /= 1_000f64;
        format!("{:.03} μs", nsecs)
    } else {
        format!("{:.03} ns", nsecs)
    }
}


/// Create a benchmark function based around `run_parallel_bench`, including printing benchmark
/// output. This is in a macro because we have to define a full `fn` in order to send the function
/// across thread boundaries and clone it.
macro_rules! create_bench {

    ($name:ident, $desc:expr, $param:tt ::: $pty:ty = $pval:expr, $timer:ident, $nthr:expr, $iters:expr, $work:expr) => {
        fn $name() {
            fn inner($param: $pty, $timer: &mut Timer) -> Vec<f64> {
                // warm up round;
                let p = $work;
                let _ = test::black_box(p);
                let mut __samples = Vec::with_capacity($iters);
                for _ in 0..$iters {
                    $timer.reset();
                    let p = $work;
                    let _ = test::black_box(p);
                    __samples.push($timer.elapsed_ns() as f64);
                }
                __samples
            }
            let params = $pval;
            let nthr = $nthr;
            let res = run_parallel_bench(params, nthr, inner);
            let stats = &res[..];
            println!("benchmark-n{:02} {:40} {:12} per iteration (+/- {:.02}%)", nthr, $desc,
                     format_dur(stats.mean()),
                     stats.median_abs_dev_pct());
        }
    };
}

/// Group a number of benchmarks created using `create_bench` for three `Vec`-like types.
macro_rules! bench_group {
    ($name:ident, $param:tt ::: $pty:ty = $pval:expr, $iters:expr, $fn:tt) => {
        fn $name() {
            create_bench!(v1,
                          format!("{}_vec", stringify!($name)),
                          $param ::: $pty = $pval,
                          _t,
                          1,
                          $iters,
                          $fn::<Vec<_>>($param, _t));
            create_bench!(v1n,
                          format!("{}_vec", stringify!($name)),
                          $param ::: $pty = $pval,
                          _t,
                          num_cpus::get(),
                          $iters,
                          $fn::<Vec<_>>($param, _t));
            create_bench!(v2,
                          format!("{}_avec_heap", stringify!($name)),
                          $param ::: $pty = $pval,
                          _t,
                          1,
                          $iters,
                          $fn::<AVec<_, Heap>>($param, _t));
            create_bench!(v2n,
                          format!("{}_avec_heap", stringify!($name)),
                          $param ::: $pty = $pval,
                          _t,
                          num_cpus::get(),
                          $iters,
                          $fn::<AVec<_, Heap>>($param, _t));
            create_bench!(v3,
                          format!("{}_avec_elf", stringify!($name)),
                          $param ::: $pty = $pval,
                          _t,
                          1,
                          $iters,
                          $fn::<AVec<_, SharedAlloc>>($param, _t));
            create_bench!(v3n,
                          format!("{}_avec_elf", stringify!($name)),
                          $param ::: $pty = $pval,
                          _t,
                          num_cpus::get(),
                          $iters,
                          $fn::<AVec<_, SharedAlloc>>($param, _t));

            v3();
            v3n();
            v1();
            v1n();
            v2();
            v2n();
        }
    };
}

#[inline(never)]
fn push_noinline<T, V: VecLike<T>>(v: &mut V, t: T) {
    v.push(t)
}

/// A benchmark testing smaller-sized allocations.
fn do_push<V: VecLike<usize> + Default>(ops: usize, _timer: &mut Timer) -> V {
    let mut v = V::default();
    for _ in 0..8 {
        for i in 0..ops {
            push_noinline(&mut v, i);
        }
        v = V::default();
    }
    v
}

/// A benchmark stressing medium-sized allocations.
fn do_push_medium<V: VecLike<usize> + Default>(ops: usize, timer: &mut Timer) -> V {
    timer.stop();
    let mut v = V::default();
    v.extend(1..(1 << 14));
    timer.resume();
    for i in 0..ops {
        push_noinline(&mut v, i);
    }
    v
}

/// A benchmark stressing large-sized allocations.
fn do_push_large<V: VecLike<[usize; 1024]> + Default>(ops: usize, _timer: &mut Timer) -> V {
    let mut v = V::default();
    for i in 0..ops {
        // noinline may cause overflow issues
        v.push([i; 1024]);
    }
    v
}

bench_group!(bench_push, ops ::: usize = 500, 250_000, do_push);
bench_group!(bench_push_medium, ops ::: usize = 500_000, 100, do_push_medium);
bench_group!(bench_push_large, ops ::: usize = 1_000, 50, do_push_large);

fn main() {
    bench_push();
    bench_push_medium();
    bench_push_large();
}
