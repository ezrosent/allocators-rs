// Copyright 2017-2018 the authors. See the 'Copyright and license' section of the
// README.md file at the top-level directory of this repository.
//
// Licensed under the Apache License, Version 2.0 (the LICENSE-APACHE file) or
// the MIT license (the LICENSE-MIT file) at your option. This file may not be
// copied, modified, or distributed except according to those terms.

#![feature(alloc)]
#![feature(allocator_api)]
extern crate alloc;
extern crate elfmalloc;
extern crate num_cpus;
use std::marker;
use alloc::heap;
use std::mem;
use std::thread;
use std::time;
use std::ptr::write_volatile;

// use elfmalloc::slag::{AllocBuilder, LocalAllocator, MagazineAllocator};
// use elfmalloc::general::global;
use elfmalloc::alloc_impl::ElfMallocGlobal;
use elfmalloc::general::DynamicAllocator;
use std::sync::{Arc, Barrier};
use std::sync::atomic::{AtomicPtr, Ordering};

type BenchItem = [usize; 2];

const PAGE_SIZE: usize = 32 << 10;
const EAGER_DECOMMIT: usize = 30 << 10;


trait AllocLike
where
    Self: Clone + Send,
{
    type Item;
    fn create() -> Self;
    unsafe fn allocate(&mut self) -> *mut Self::Item;
    unsafe fn deallocate(&mut self, *mut Self::Item);
    fn kill(&mut self) {}
}

// impl<T: 'static> AllocLike for MagazineAllocator<T> {
//     type Item = T;
//     fn create() -> Self {
//         AllocBuilder::default()
//             .cutoff_factor(0.8)
//             .page_size(PAGE_SIZE)
//             .eager_decommit_threshold(EAGER_DECOMMIT)
//             .build_magazine()
//     }
//
//     unsafe fn allocate(&mut self) -> *mut T {
//         self.alloc()
//     }
//
//     unsafe fn deallocate(&mut self, item: *mut T) {
//         self.free(item)
//     }
//     fn kill(&mut self) {}
// }

struct ElfGlobal<T>(marker::PhantomData<T>);
impl<T> Clone for ElfGlobal<T> {
    fn clone(&self) -> Self {
        ElfGlobal(marker::PhantomData)
    }
}
unsafe impl<T> Send for ElfGlobal<T> {}

use alloc::allocator::{Alloc, Layout};

impl<T: 'static> AllocLike for ElfGlobal<T> {
    type Item = T;
    fn create() -> Self {
        ElfGlobal(marker::PhantomData)
    }

    unsafe fn allocate(&mut self) -> *mut T {
        (&ElfMallocGlobal{}).alloc(Layout::new::<T>()).unwrap() as *mut T
        // global::alloc(mem::size_of::<T>()) as *mut T
    }

    unsafe fn deallocate(&mut self, item: *mut T) {
        (&ElfMallocGlobal{}).dealloc(item as *mut u8, Layout::new::<T>())
        // global::free(item as *mut u8)
    }

    fn kill(&mut self) {}
}

struct ElfClone<T>(DynamicAllocator, marker::PhantomData<T>);
impl<T> Clone for ElfClone<T> {
    fn clone(&self) -> Self {
        ElfClone(self.0.clone(), marker::PhantomData)
    }
}
unsafe impl<T> Send for ElfClone<T> {}

impl<T: 'static> AllocLike for ElfClone<T> {
    type Item = T;
    fn create() -> Self {
        ElfClone(DynamicAllocator::new().unwrap(), marker::PhantomData)
    }

    unsafe fn allocate(&mut self) -> *mut T {
        // TODO: Do something other than unwrap?
        self.0.alloc(mem::size_of::<T>()).unwrap() as *mut T
    }

    unsafe fn deallocate(&mut self, item: *mut T) {
        self.0.free(item as *mut u8)
    }

    fn kill(&mut self) {}
}

// impl<T: 'static> AllocLike for LocalAllocator<T> {
//     type Item = T;
//     fn create() -> Self {
//         AllocBuilder::default()
//             .cutoff_factor(0.8)
//             .page_size(PAGE_SIZE)
//             .eager_decommit_threshold(EAGER_DECOMMIT)
//             .build_local()
//     }
//
//     unsafe fn allocate(&mut self) -> *mut T {
//         self.alloc()
//     }
//
//     unsafe fn deallocate(&mut self, item: *mut T) {
//         self.free(item)
//     }
//     fn kill(&mut self) {}
// }

struct DefaultMalloc<T>(marker::PhantomData<T>);

unsafe impl<T> Send for DefaultMalloc<T> {}

impl<T> Clone for DefaultMalloc<T> {
    fn clone(&self) -> Self {
        DefaultMalloc(marker::PhantomData)
    }
}

impl<T> AllocLike for DefaultMalloc<T> {
    type Item = T;
    fn create() -> Self {
        DefaultMalloc(marker::PhantomData)
    }
    unsafe fn allocate(&mut self) -> *mut T {
        use heap::{Alloc, Layout};
        heap::Heap
            .alloc(Layout::from_size_align(mem::size_of::<T>(), 8).unwrap())
            .unwrap() as *mut T
    }

    unsafe fn deallocate(&mut self, item: *mut T) {
        use heap::{Alloc, Layout};
        heap::Heap.dealloc(
            item as *mut u8,
            Layout::from_size_align(mem::size_of::<T>(), 8).unwrap(),
        );
    }
}


macro_rules! time_block {
    ($block:expr) => {
        {
            // warm up
            $block;
            let start = time::Instant::now();
            $block;
            let dur = start.elapsed();
            (dur.as_secs() * 1_000_000_000) + u64::from(dur.subsec_nanos())
        }
    }
}

macro_rules! time_block_once {
    ($block:expr) => {
        {
            let start = time::Instant::now();
            $block;
            let dur = start.elapsed();
            (dur.as_secs() * 1_000_000_000) + u64::from(dur.subsec_nanos())
        }
    }
}

fn bench_alloc_free_pairs<A: AllocLike<Item = BenchItem> + 'static>(
    nthreads: usize,
    per_thread: usize,
) {
    let mut a = A::create();
    let b = Arc::new(Barrier::new(nthreads + 1));
    let mut threads = Vec::new();
    for _ in 0..nthreads {
        let mut alloc = a.clone();
        let barrier = b.clone();
        threads.push(thread::spawn(move || {
            barrier.wait();
            // warmup
            time_block!(unsafe {
                            for i in 0..per_thread {
                                let ptr = alloc.allocate();
                                write_volatile(ptr as *mut usize, i);
                                alloc.deallocate(ptr);
                            }
                        })
        }));
    }
    b.wait();
    let mut total = 0;
    for i in threads {
        total += i.join().unwrap();
    }

    // why nthreads * nthreads? Total is actually n_threads * mean time, so we need an extra
    // nthreads factor to not over-count the time it takes to perform the workload.
    println!(
        "{} Mops/s",
        ((nthreads * nthreads * per_thread * 2 * 1_000) as f64) / (total as f64)
    );
    a.kill();
}

fn bench_alloc_free_pairs_buffered<A: AllocLike<Item = BenchItem> + 'static>(
    nthreads: usize,
    per_thread: usize,
) {
    let mut a = A::create();
    let b = Arc::new(Barrier::new(nthreads + 1));
    let mut threads = Vec::new();
    for _ in 0..nthreads {
        let mut alloc = a.clone();
        let barrier = b.clone();
        threads.push(thread::spawn(move || {
            let mut ptrs = Vec::new();
            for _ in 0..(64 * 1024) {
                ptrs.push(unsafe { alloc.allocate() });
            }

            barrier.wait();
            // warmup
            time_block!(unsafe {
                            for i in 0..per_thread {
                                let idx = i % ptrs.len();
                                let ptr = ptrs.get_unchecked_mut(idx);
                                alloc.deallocate(*ptr);
                                *ptr = alloc.allocate();
                                write_volatile(*ptr as *mut u8, i as u8);
                            }
                        })
        }));
    }
    b.wait();
    let mut total = 0;
    for i in threads {
        total += i.join().unwrap();
    }

    // why nthreads * nthreads? Total is actually n_threads * mean time, so we need an extra
    // nthreads factor to not over-count the time it takes to perform the workload.
    println!(
        "{} Mops/s",
        ((nthreads * nthreads * per_thread * 2 * 1_000) as f64) / (total as f64)
    );
    a.kill();
}

fn bench_prod_cons<A: AllocLike<Item = BenchItem> + 'static>(nthreads: usize, per_thread: usize) {
    let mut a = A::create();
    let b = Arc::new(Barrier::new(nthreads + 1));
    let mut v_base = Vec::new();
    for _ in 0..nthreads {
        v_base.push(AtomicPtr::new(
            Box::into_raw(Box::new(Vec::with_capacity(per_thread))),
        ));
    }
    let v = Arc::new(v_base);
    let mut threads = Vec::new();
    for t in 0..nthreads {
        let mut alloc = a.clone();
        let barrier = b.clone();
        let io_vec = v.clone();
        threads.push(thread::spawn(move || {
            let (me, them) = (t, (t + 1) % nthreads);
            unsafe {
                let my_v = io_vec[me].load(Ordering::Acquire).as_mut().unwrap();
                for i in 0..per_thread {
                    let ptr = alloc.allocate();
                    write_volatile(ptr as *mut usize, i);
                    my_v.push(AtomicPtr::new(ptr));
                }
            }
            barrier.wait();
            time_block_once!(unsafe {
                                 for i in 0..per_thread {
                                     let ptr =
                                         io_vec[them].load(Ordering::Acquire).as_ref().unwrap()[i]
                                             .load(Ordering::Relaxed);
                                     alloc.deallocate(ptr);
                                 }
                             })
        }));
    }
    b.wait();
    let mut total = 0;
    for i in threads {
        total += i.join().unwrap();
    }
    println!(
        "{} Mops/s",
        ((nthreads * nthreads * per_thread * 1_000) as f64) / (total as f64)
    );
    a.kill();
}

fn bench_alloc_free<A: AllocLike<Item = BenchItem> + 'static>(nthreads: usize, per_thread: usize) {
    let mut a = A::create();
    let b = Arc::new(Barrier::new(nthreads + 1));
    let mut threads = Vec::new();
    for _ in 0..nthreads {
        let mut alloc = a.clone();
        let barrier = b.clone();
        threads.push(thread::spawn(move || {
            barrier.wait();
            // warmup
            let mut ptrs = Vec::with_capacity(2 * per_thread);
            time_block_once!(unsafe {
                                 for i in 0..per_thread {
                                     let ptr = alloc.allocate();
                                     write_volatile(ptr as *mut usize, i);
                                     ptrs.push(ptr);
                                 }
                                 for ptr in ptrs {
                                     alloc.deallocate(ptr);
                                 }
                             })
        }));
    }
    b.wait();
    let mut total = 0;
    for i in threads {
        total += i.join().unwrap();
    }
    println!(
        "{} Mops/s",
        ((nthreads * nthreads * per_thread * 2 * 1_000) as f64) / (total as f64)
    );
    a.kill()
}

fn bench_alloc<A: AllocLike<Item = BenchItem> + 'static>(nthreads: usize, per_thread: usize) {
    let mut a = A::create();
    let b = Arc::new(Barrier::new(nthreads + 1));
    let mut threads = Vec::new();
    for _ in 0..nthreads {
        let mut alloc = a.clone();
        let barrier = b.clone();
        threads.push(thread::spawn(move || {
            barrier.wait();
            // warmup
            let mut ptrs = Vec::with_capacity(per_thread);
            let t = time_block_once!(unsafe {
                                         for i in 0..per_thread {
                                             let ptr = alloc.allocate();
                                             write_volatile(ptr as *mut usize, i);
                                             ptrs.push(ptr);
                                         }
                                     });
            for ptr in ptrs {
                unsafe { alloc.deallocate(ptr) };
            }
            t
        }));
    }
    b.wait();
    let mut total = 0;
    for i in threads {
        total += i.join().unwrap();
    }
    println!(
        "{} Mops/s",
        ((nthreads * nthreads * per_thread * 1_000) as f64) / (total as f64)
    );
    a.kill()
}

fn bench_free<A: AllocLike<Item = BenchItem> + 'static>(nthreads: usize, per_thread: usize) {
    let mut a = A::create();
    let b = Arc::new(Barrier::new(nthreads + 1));
    let mut threads = Vec::new();
    for _ in 0..nthreads {
        let mut alloc = a.clone();
        let barrier = b.clone();
        threads.push(thread::spawn(move || {
            barrier.wait();
            // warmup
            let mut ptrs = Vec::with_capacity(per_thread);
            for i in 0..per_thread {
                unsafe {
                    let ptr = alloc.allocate();
                    write_volatile(ptr as *mut usize, i);
                    ptrs.push(ptr);
                }
            }
            time_block_once!(unsafe {
                                 for ptr in ptrs {
                                     alloc.deallocate(ptr);
                                 }
                             })
        }));
    }
    b.wait();
    let mut total = 0;
    for i in threads {
        total += i.join().unwrap();
    }
    println!(
        "{} Mops/s",
        ((nthreads * nthreads * per_thread * 1_000) as f64) / (total as f64)
    );
    a.kill()
}

macro_rules! run_bench_inner {
    ($bench:tt, $nthreads:expr, $iters:expr) => {
        let iters = $iters;
        let nthreads = $nthreads;
        // println!("global malloc");
        // $bench::<DefaultMalloc<BenchItem>>(nthreads, iters);
        println!("global slag allocator");
        $bench::<ElfGlobal<BenchItem>>(nthreads, iters);
        // println!("clone-based slag allocator");
        // $bench::<ElfClone<BenchItem>>(nthreads, iters);
        // println!("slag allocator");
        // $bench::<LocalAllocator<BenchItem>>(nthreads, iters);
        // println!("slagazine allocator");
        // $bench::<MagazineAllocator<BenchItem>>(nthreads, iters);
    };
}

macro_rules! run_bench {
    (both $desc:expr, $bench:tt, $nthreads:expr, $iters:expr) => {
        println!("\n{} - {}", $desc, "single-threaded");
        run_bench_inner!($bench, 1, $iters);
        println!("\n{} - {} threads", $desc, $nthreads);
        run_bench_inner!($bench, $nthreads, $iters);
    };

    (threads $desc:expr, $bench:tt, $nthreads:expr, $iters:expr) => {
        println!("\n{} - {} threads", $desc, $nthreads);
        run_bench_inner!($bench, $nthreads, $iters);
    };
}

fn main() {
    const ITERS: usize = 1_000_000;
    let nthreads = num_cpus::get();
    println!(
        "allocating {} bytes per thread",
        ITERS * mem::size_of::<BenchItem>()
    );

    run_bench!(both "alloc/free pairs", bench_alloc_free_pairs, nthreads, ITERS);
    run_bench!(both "buffered alloc/free pairs", bench_alloc_free_pairs_buffered, nthreads, ITERS);
    // run_bench!(both "alloc (thread-local)", bench_alloc, nthreads, ITERS);
    // run_bench!(both "free (thread-local)", bench_free, nthreads, ITERS);
    // run_bench!(both "alloc & free (thread-local)", bench_alloc_free, nthreads, ITERS);
    // run_bench!(threads "free (producer-consumer)", bench_prod_cons, nthreads, ITERS);
}
