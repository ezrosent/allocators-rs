// Copyright 2017-2018 the authors. See the 'Copyright and license' section of the
// README.md file at the top-level directory of this repository.
//
// Licensed under the Apache License, Version 2.0 (the LICENSE-APACHE file) or
// the MIT license (the LICENSE-MIT file) at your option. This file may not be
// copied, modified, or distributed except according to those terms.

extern crate bagpipe;
extern crate crossbeam;
extern crate num_cpus;
use crossbeam::mem::epoch;
use crossbeam::sync::{MsQueue, TreiberStack};
use bagpipe::bag::{WeakBag, SharedWeakBag, PopStatus, ArcLike};
use bagpipe::queue::{GeneralYC, YangCrummeyQueue, FAAQueueLowLevel, FAAArrayQueue};
use std::sync::{Arc, Barrier};
use std::time;
use std::thread;

type BagPipe<T: SharedWeakBag> = bagpipe::BagPipe<T, bagpipe::DummyCleanup<T::Item>>;

struct WorkloadStats {
    nthreads: usize,
    total_ops: usize,
    time_nsecs: u64,
    description: String,
    failed_push: usize,
    failed_pop: usize,
    prefill: usize,
}

impl WorkloadStats {
    pub fn print(&self) {
        println!("{}: {} threads, {} Mops/s {} failed pushes {} failed pops. Prefilled {}",
                 self.description,
                 self.nthreads,
                 (self.total_ops as f64) / ((self.time_nsecs >> 10) as f64),
                 self.failed_push,
                 self.failed_pop,
                 self.prefill);
    }
}

fn since_then(i: time::Instant) -> u64 {
    let dur = i.elapsed();
    (dur.as_secs() * 1_000_000_000) + (dur.subsec_nanos() as u64)
}

/// Perform a standard enqueuers-dequerers benchmark with optional
/// pre-filling of the bag.
fn enqueue_dequeue_pairs_usize<W>(npairs: usize,
                                  eq_per_thread: usize,
                                  prefill_with: usize,
                                  description: String)
                                  -> WorkloadStats
    where W: WeakBag<Item = usize> + Send + 'static + Default
{
    let wb = W::default();
    for i in 0..prefill_with {
        wb.clone().push_mut(i);
    }
    let barrier = Arc::new(Barrier::new(npairs * 2 + 1));
    let mut threads = Vec::new();
    let mut res = WorkloadStats {
        nthreads: npairs * 2,
        total_ops: (eq_per_thread * 2 * // enqueue-dequeue pairs per thread = 2 ops
                    npairs * 2 /* pairs of threads = 2 threads */),
        time_nsecs: 0,
        failed_push: 0,
        failed_pop: 0,
        description: description,
        prefill: prefill_with,
    };

    for tnum in 0..(2 * npairs) {
        let (mut bag, lbar) = (wb.clone(), barrier.clone());
        threads.push(thread::spawn(move || {
            let _g = epoch::pin();
            let mut push_failures = 0;
            let mut pop_failures = 0;
            lbar.wait();
            // warm-up period
            for i in 0..(eq_per_thread / 2) {
                bag.push_mut(tnum * npairs + i);
                let _ = bag.try_pop_mut();
            }
            let start = time::Instant::now();
            for i in 0..eq_per_thread {
                if bag.try_push_mut(tnum * npairs + i).is_err() {
                    push_failures += 1;
                }
                if let Err(PopStatus::TransientFailure) = bag.try_pop_mut() {
                    pop_failures += 1;
                }
            }
            (push_failures, pop_failures, since_then(start))
        }));
    }

    barrier.wait();
    let mut total_nsecs = 0;
    for t in threads {
        let (pushes, pops, nsecs) = t.join().unwrap();
        res.failed_push += pushes;
        res.failed_pop += pops;
        total_nsecs += nsecs;
    }
    res.time_nsecs = total_nsecs / (res.nthreads as u64);
    res
}

fn enqueue_dequeue_pairs_strong<W>(npairs: usize,
                                   eq_per_thread: usize,
                                   prefill_with: usize,
                                   description: String)
                                   -> WorkloadStats
    where W: WeakBag<Item = usize> + Send + 'static + Default
{
    let wb = W::default();
    for i in 0..prefill_with {
        wb.clone().push_mut(i);
    }
    let barrier = Arc::new(Barrier::new(npairs * 2 + 1));
    let mut threads = Vec::new();
    let mut res = WorkloadStats {
        nthreads: npairs * 2,
        total_ops: (eq_per_thread * 2 * // enqueue-dequeue pairs per thread = 2 ops
                    npairs * 2 /* pairs of threads = 2 threads */),
        time_nsecs: 0,
        failed_push: 0,
        failed_pop: 0,
        description: description,
        prefill: prefill_with,
    };

    for tnum in 0..(2 * npairs) {
        let (mut bag, lbar) = (wb.clone(), barrier.clone());
        threads.push(thread::spawn(move || {
            let _g = epoch::pin();
            lbar.wait();
            // warm-up period
            for i in 0..(eq_per_thread / 2) {
                bag.push_mut(tnum * npairs + i);
                let _ = bag.try_pop_mut();
            }
            let start = time::Instant::now();
            for i in 0..eq_per_thread {
                bag.push_mut(tnum * npairs + i);
                bag.pop_mut();

            }
            since_then(start)
        }));
    }

    barrier.wait();
    let mut total_nsecs = 0;
    for t in threads {
        total_nsecs += t.join().unwrap();
    }
    res.time_nsecs = total_nsecs / (res.nthreads as u64);
    res
}

/// Perform a standard enqueuers-dequerers benchmark with optional
/// pre-filling of the bag and no failures.
fn producer_consumer_strong<W>(npairs: usize,
                               ops_per_thread: usize,
                               prefill_with: usize,
                               description: String)
                               -> WorkloadStats
    where W: WeakBag<Item = usize> + Send + 'static + Default
{
    let wb = W::default();
    for i in 0..prefill_with {
        wb.clone().push_mut(i);
    }
    let barrier = Arc::new(Barrier::new(npairs * 2 + 1));
    let mut push_threads = Vec::new();
    let mut pop_threads = Vec::new();
    let mut res = WorkloadStats {
        nthreads: npairs * 2,
        total_ops: ops_per_thread * npairs * 2,
        time_nsecs: 0,
        failed_push: 0,
        failed_pop: 0,
        description: description,
        prefill: prefill_with,
    };

    for tnum in 0..npairs {
        let (mut bag, lbar) = (wb.clone(), barrier.clone());
        push_threads.push(thread::spawn(move || {
            let _g = epoch::pin();
            lbar.wait();
            for i in 0..(ops_per_thread / 2) {
                bag.push_mut(tnum * npairs + i);
            }
            let start = time::Instant::now();
            for i in 0..ops_per_thread {
                bag.push_mut(tnum * npairs + i);
            }
            since_then(start)
        }));
    }

    for _ in 0..npairs {
        let (mut bag, lbar) = (wb.clone(), barrier.clone());
        pop_threads.push(thread::spawn(move || {
            let _g = epoch::pin();
            lbar.wait();
            for _ in 0..(ops_per_thread / 2) {
                let _ = bag.try_pop_mut();
            }
            let start = time::Instant::now();
            for _ in 0..ops_per_thread {
                bag.pop_mut();
            }
            since_then(start)
        }));
    }
    barrier.wait();
    let mut total_dur = 0;
    for t in push_threads {
        total_dur += t.join().unwrap();
    }
    for t in pop_threads {
        total_dur += t.join().unwrap();
    }
    res.time_nsecs = total_dur / (res.nthreads as u64);
    res
}


/// Perform a standard enqueuers-dequerers benchmark with optional
/// pre-filling of the bag.
fn enqueue_dequeue_usize<W>(npairs: usize,
                            ops_per_thread: usize,
                            prefill_with: usize,
                            description: String)
                            -> WorkloadStats
    where W: WeakBag<Item = usize> + Send + 'static + Default
{
    let wb = W::default();
    for i in 0..prefill_with {
        wb.clone().push_mut(i);
    }
    let barrier = Arc::new(Barrier::new(npairs * 2 + 1));
    let mut push_threads = Vec::new();
    let mut pop_threads = Vec::new();
    let mut res = WorkloadStats {
        nthreads: npairs * 2,
        total_ops: ops_per_thread * npairs * 2,
        time_nsecs: 0,
        failed_push: 0,
        failed_pop: 0,
        description: description,
        prefill: prefill_with,
    };

    for tnum in 0..npairs {
        let (mut bag, lbar) = (wb.clone(), barrier.clone());
        push_threads.push(thread::spawn(move || {
            let _g = epoch::pin();
            let mut failures = 0;
            lbar.wait();
            for i in 0..(ops_per_thread / 2) {
                bag.push_mut(tnum * npairs + i);
            }
            let start = time::Instant::now();
            for i in 0..ops_per_thread {
                if bag.try_push_mut(tnum * npairs + i).is_err() {
                    failures += 1;
                }
            }
            (failures, since_then(start))
        }));
    }

    for _ in 0..npairs {
        let (mut bag, lbar) = (wb.clone(), barrier.clone());
        pop_threads.push(thread::spawn(move || {
            let _g = epoch::pin();
            let mut failures = 0;
            lbar.wait();
            for _ in 0..(ops_per_thread / 2) {
                let _ = bag.try_pop_mut();
            }
            let start = time::Instant::now();
            for _ in 0..ops_per_thread {
                match bag.try_pop_mut() {
                    Ok(_) |
                    Err(PopStatus::Empty) => {}
                    Err(PopStatus::TransientFailure) => failures += 1,
                }
            }
            (failures, since_then(start))
        }));
    }
    barrier.wait();
    let mut total_dur = 0;
    for t in push_threads {
        let (fails, dur) = t.join().unwrap();
        res.failed_push += fails;
        total_dur += dur;
    }
    for t in pop_threads {
        let (fails, dur) = t.join().unwrap();
        res.failed_pop += fails;
        total_dur += dur;
    }
    res.time_nsecs = total_dur / (res.nthreads as u64);
    res
}


// TODO(ezrosent) make this var-args to make invocation cleaner.
macro_rules! print_bench {

    (bp, $ds:ident, $ops:expr, $pref:expr, $bench:ident) => {
        for _ in 0..10 {
            let _ = epoch::pin();
        }
        {
            let _g = epoch::pin();
            let _pref = $pref;
            $bench::<BagPipe<$ds<usize>>>(
                num_cpus::get()/2, $ops, _pref, format!("bp-{}", stringify!($ds)).to_string()).print();
        }
    };

    ($ds:ident, $ops:expr, $prefill:expr, $bench:ident) => {
        for _ in 0..10 {
            let _ = epoch::pin();
        }
        {
            let _g = epoch::pin();
            let _pref = $prefill;
            $bench::<ArcLike<$ds<usize>>>(num_cpus::get()/2, $ops, _pref,
            stringify!($ds).to_string()).print();
        }
    };

    ($ds:ident, $ops:expr, $bench:ident) => {
        print_bench!($ds, $ops, 0, $bench);
    };

}

fn eq_strong() {

    println!("\nEnqueue-Dequeue Strong No Prefilling\n");
    print_bench!(bp,
                 YangCrummeyQueue,
                 1 << 20,
                 0,
                 enqueue_dequeue_pairs_strong);
    print_bench!(bp,
                 FAAQueueLowLevel,
                 1 << 20,
                 0,
                 enqueue_dequeue_pairs_strong);
    print_bench!(FAAQueueLowLevel, 1 << 20, 0, enqueue_dequeue_pairs_strong);
    print_bench!(FAAArrayQueue, 1 << 20, 0, enqueue_dequeue_pairs_strong);
    print_bench!(YangCrummeyQueue, 1 << 20, 0, enqueue_dequeue_pairs_strong);
    print_bench!(GeneralYC, 1 << 20, 0, enqueue_dequeue_pairs_strong);
    print_bench!(TreiberStack, 1 << 20, 0, enqueue_dequeue_pairs_strong);
    print_bench!(MsQueue, 1 << 20, 0, enqueue_dequeue_pairs_strong);

}

fn eq() {

    println!("\nEnqueue-Dequeue No Prefilling\n");
    print_bench!(bp,
                 YangCrummeyQueue,
                 1 << 20,
                 0,
                 enqueue_dequeue_pairs_usize);
    print_bench!(bp,
                 FAAQueueLowLevel,
                 1 << 20,
                 0,
                 enqueue_dequeue_pairs_usize);
    print_bench!(FAAQueueLowLevel, 1 << 20, 0, enqueue_dequeue_pairs_usize);
    print_bench!(FAAArrayQueue, 1 << 20, 0, enqueue_dequeue_pairs_usize);
    print_bench!(YangCrummeyQueue, 1 << 20, 0, enqueue_dequeue_pairs_usize);
    print_bench!(GeneralYC, 1 << 20, 0, enqueue_dequeue_pairs_usize);
    print_bench!(TreiberStack, 1 << 20, 0, enqueue_dequeue_pairs_usize);
    print_bench!(MsQueue, 1 << 20, 0, enqueue_dequeue_pairs_usize);

}

fn eq_prefill() {

    println!("\nEnqueue-Dequeue Prefilling\n");
    print_bench!(bp,
                 YangCrummeyQueue,
                 1 << 20,
                 1 << 10,
                 enqueue_dequeue_pairs_usize);
    print_bench!(bp,
                 FAAQueueLowLevel,
                 1 << 20,
                 1 << 10,
                 enqueue_dequeue_pairs_usize);
    print_bench!(FAAQueueLowLevel,
                 1 << 20,
                 1 << 10,
                 enqueue_dequeue_pairs_usize);
    print_bench!(FAAArrayQueue, 1 << 20, 1 << 10, enqueue_dequeue_pairs_usize);
    print_bench!(YangCrummeyQueue,
                 1 << 20,
                 1 << 10,
                 enqueue_dequeue_pairs_usize);
    print_bench!(GeneralYC, 1 << 20, 1 << 10, enqueue_dequeue_pairs_usize);
    // print_bench!(TreiberStack, 1 << 20, 1 << 10, enqueue_dequeue_pairs_usize);
    // print_bench!(MsQueue, 1 << 20, 1 << 10, enqueue_dequeue_pairs_usize);

}

fn prod_consume() {

    println!("\nProducer-Consumer Strong No Prefilling\n");
    print_bench!(bp, YangCrummeyQueue, 1 << 20, 0, producer_consumer_strong);
    print_bench!(bp, FAAQueueLowLevel, 1 << 20, 0, producer_consumer_strong);
    print_bench!(FAAQueueLowLevel, 1 << 20, producer_consumer_strong);
    print_bench!(FAAArrayQueue, 1 << 20, producer_consumer_strong);
    print_bench!(YangCrummeyQueue, 1 << 20, producer_consumer_strong);
    print_bench!(GeneralYC, 1 << 20, producer_consumer_strong);
    // print_bench!(TreiberStack, 1 << 20, producer_consumer_strong);
    // print_bench!(MsQueue, 1 << 20, producer_consumer_strong);



    println!("\nProducer-Consumer No Prefilling\n");
    print_bench!(bp, YangCrummeyQueue, 1 << 20, 0, enqueue_dequeue_usize);
    print_bench!(bp, FAAQueueLowLevel, 1 << 20, 0, enqueue_dequeue_usize);
    print_bench!(FAAQueueLowLevel, 1 << 20, enqueue_dequeue_usize);
    print_bench!(FAAArrayQueue, 1 << 20, enqueue_dequeue_usize);
    print_bench!(YangCrummeyQueue, 1 << 20, enqueue_dequeue_usize);
    print_bench!(GeneralYC, 1 << 20, enqueue_dequeue_usize);
    // print_bench!(TreiberStack, 1 << 20, enqueue_dequeue_usize);
    // print_bench!(MsQueue, 1 << 20, enqueue_dequeue_usize);

    println!("\nProducer-Consumer Prefilling\n");
    print_bench!(bp,
                 YangCrummeyQueue,
                 1 << 20,
                 1 << 10,
                 enqueue_dequeue_usize);
    print_bench!(bp,
                 FAAQueueLowLevel,
                 1 << 20,
                 1 << 10,
                 enqueue_dequeue_usize);
    print_bench!(FAAQueueLowLevel, 1 << 20, 1 << 10, enqueue_dequeue_usize);
    print_bench!(FAAArrayQueue, 1 << 20, 1 << 10, enqueue_dequeue_usize);
    print_bench!(YangCrummeyQueue, 1 << 20, 1 << 10, enqueue_dequeue_usize);
    print_bench!(GeneralYC, 1 << 20, 1 << 10, enqueue_dequeue_usize);
    // print_bench!(TreiberStack, 1 << 20, 1 << 10, enqueue_dequeue_usize);
    // print_bench!(MsQueue, 1 << 20, 1 << 10, enqueue_dequeue_usize);
}

fn main() {
    eq_strong();
    eq();
    eq_prefill();
    prod_consume();
}
