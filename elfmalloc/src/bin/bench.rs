#![feature(test)]
#![feature(alloc)]
#![feature(allocator_api)]
extern crate test;
extern crate elfmalloc;
extern crate bagpipe;
extern crate alloc;
extern crate num_cpus;
extern crate slab_alloc;
extern crate object_alloc;
use std::marker;
use alloc::heap;
use std::mem;
use std::thread;
use std::time;
use std::ptr::write_volatile;

use elfmalloc::slag::{LocalAllocator, MagazineAllocator};
use elfmalloc::general::global;
use elfmalloc::general::DynamicAllocator;
use std::sync::{Arc, Barrier, Mutex};
use std::sync::atomic::{AtomicPtr, Ordering};

use slab_alloc::{SlabAllocBuilder, SlabAlloc, HeapBackingAlloc, NopInitSystem};
use object_alloc::ObjectAlloc;

type BenchItem = [usize; 2];

const PAGE_SIZE: usize = 32 << 10;
const EAGER_DECOMMIT: usize = 30 << 10;
const USABLE_SIZE: usize = 32 << 10;

struct SerialSlabAlloc<T>(Arc<Mutex<SlabAlloc<T, NopInitSystem, HeapBackingAlloc>>>);
unsafe impl<T> Send for SerialSlabAlloc<T> {}

impl<T> Clone for SerialSlabAlloc<T> {
    fn clone(&self) -> Self {
        SerialSlabAlloc(self.0.clone())
    }
}


trait SerialAlloc {
    type Item;
    fn create() -> Self;
    unsafe fn allocate(&mut self) -> *mut Self::Item;
    unsafe fn deallocate(&mut self, *mut Self::Item);
    fn kill(&mut self) {}
}

trait AllocLike where Self: SerialAlloc + Send + Clone {}

impl<T: 'static> SerialAlloc for SlabAlloc<T, NopInitSystem, HeapBackingAlloc> {
    type Item = T;

    fn create() -> Self {
        unsafe { SlabAllocBuilder::no_initialize().build() }
    }

    unsafe fn allocate(&mut self) -> *mut T {
        self.alloc().expect("slab alloc should not fail")
    }

    unsafe fn deallocate(&mut self, item: *mut T) {
        self.dealloc(item)
    }
}


impl<T: 'static> AllocLike for SerialSlabAlloc<T> {}
impl<T: 'static> SerialAlloc for SerialSlabAlloc<T> {
    type Item = T;
    fn create() -> Self {
        unsafe { SerialSlabAlloc(Arc::new(Mutex::new(SlabAllocBuilder::no_initialize().build()))) }
    }

    unsafe fn allocate(&mut self) -> *mut T {
        self.0.lock().unwrap().alloc().expect("slaballoc should not fail")
    }

    unsafe fn deallocate(&mut self, item: *mut T) {
        self.0.lock().unwrap().dealloc(item)
    }
}

impl<T: 'static> AllocLike for MagazineAllocator<T> {}
impl<T: 'static> SerialAlloc for MagazineAllocator<T> {
    type Item = T;
    fn create() -> Self {
        Self::new_standalone(0.8,
                             PAGE_SIZE,
                             4 << 30,
                             1 << 40,
                             EAGER_DECOMMIT,
                             USABLE_SIZE)
    }

    unsafe fn allocate(&mut self) -> *mut T {
        self.alloc()
    }

    unsafe fn deallocate(&mut self, item: *mut T) {
        self.free(item)
    }
    fn kill(&mut self) {}
}

struct ElfGlobal<T>(marker::PhantomData<T>);
impl<T> Clone for ElfGlobal<T> {
    fn clone(&self) -> Self {
        ElfGlobal(marker::PhantomData)
    }
}
unsafe impl<T> Send for ElfGlobal<T> {}

impl<T: 'static> AllocLike for ElfGlobal<T> {}
impl<T: 'static> SerialAlloc for ElfGlobal<T> {
    type Item = T;
    fn create() -> Self {
        ElfGlobal(marker::PhantomData)
    }

    unsafe fn allocate(&mut self) -> *mut T {
        global::alloc(mem::size_of::<T>()) as *mut T
    }

    unsafe fn deallocate(&mut self, item: *mut T) {
        global::free(item as *mut u8)
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

impl<T: 'static> AllocLike for ElfClone<T> {}
impl<T: 'static> SerialAlloc for ElfClone<T> {
    type Item = T;
    fn create() -> Self {
        ElfClone(DynamicAllocator::new(), marker::PhantomData)
    }

    unsafe fn allocate(&mut self) -> *mut T {
        self.0.alloc(mem::size_of::<T>()) as *mut T
    }

    unsafe fn deallocate(&mut self, item: *mut T) {
        self.0.free(item as *mut u8)
    }

    fn kill(&mut self) {}
}

impl<T: 'static> AllocLike for LocalAllocator<T> {}
impl<T: 'static> SerialAlloc for LocalAllocator<T> {
    type Item = T;
    fn create() -> Self {
        // TODO working set algorithm
        Self::new_standalone(0.8,
                             PAGE_SIZE,
                             4 << 30,
                             1 << 40,
                             EAGER_DECOMMIT,
                             USABLE_SIZE)
    }

    unsafe fn allocate(&mut self) -> *mut T {
        self.alloc()
    }

    unsafe fn deallocate(&mut self, item: *mut T) {
        self.free(item)
    }
    fn kill(&mut self) {}
}

struct DefaultMalloc<T>(marker::PhantomData<T>);

unsafe impl<T> Send for DefaultMalloc<T> {}

impl<T> Clone for DefaultMalloc<T> {
    fn clone(&self) -> Self {
        DefaultMalloc(marker::PhantomData)
    }
}

impl<T: 'static> AllocLike for DefaultMalloc<T> {}
impl<T> SerialAlloc for DefaultMalloc<T> {
    type Item = T;
    fn create() -> Self {
        DefaultMalloc(marker::PhantomData)
    }
    unsafe fn allocate(&mut self) -> *mut T {
        use heap::{Alloc, Layout};
        heap::Heap.alloc(Layout::from_size_align(mem::size_of::<T>(), 8).unwrap())
            .unwrap() as *mut T
    }

    unsafe fn deallocate(&mut self, item: *mut T) {
        use heap::{Alloc, Layout};
        heap::Heap.dealloc(item as *mut u8,
                           Layout::from_size_align(mem::size_of::<T>(), 8).unwrap());
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
            (dur.as_secs() * 1_000_000_000) + (dur.subsec_nanos() as u64)
        }
    }
}

macro_rules! time_block_once {
    ($block:expr) => {
        {
            let start = time::Instant::now();
            $block;
            let dur = start.elapsed();
            (dur.as_secs() * 1_000_000_000) + (dur.subsec_nanos() as u64)
        }
    }
}

fn bench_alloc_free_pairs<A: AllocLike<Item = BenchItem> + 'static>(nthreads: usize,
                                                                    per_thread: usize) {
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
    println!("{} Mops/s",
             ((nthreads * nthreads * per_thread * 2 * 1_000) as f64) / (total as f64));
    a.kill();
}

fn bench_alloc_free_pairs_buffered<A: AllocLike<Item = BenchItem> + 'static>(nthreads: usize,
                                                                             per_thread: usize) {
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
            let res = time_block!(unsafe {
                for i in 0..per_thread {
                    let idx = i % ptrs.len();
                    let ptr = ptrs.get_unchecked_mut(idx);
                    alloc.deallocate(*ptr);
                    *ptr = alloc.allocate();
                    write_volatile(*ptr as *mut u8, i as u8);
                }
            });
            while let Some(p) = ptrs.pop() {
                unsafe {
                    alloc.deallocate(p)
                }
            }
            res
        }));
    }
    b.wait();
    let mut total = 0;
    for i in threads {
        total += i.join().unwrap();
    }

    // why nthreads * nthreads? Total is actually n_threads * mean time, so we need an extra
    // nthreads factor to not over-count the time it takes to perform the workload.
    println!("{} Mops/s",
             ((nthreads * nthreads * per_thread * 2 * 1_000) as f64) / (total as f64));
    a.kill();
}

fn bench_prod_cons<A: AllocLike<Item = BenchItem> + 'static>(nthreads: usize, per_thread: usize) {
    let mut a = A::create();
    let b = Arc::new(Barrier::new(nthreads + 1));
    let mut v_base = Vec::new();
    for _ in 0..nthreads {
        v_base.push(AtomicPtr::new(Box::into_raw(Box::new(Vec::with_capacity(per_thread)))));
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
                let mut my_v = io_vec[me].load(Ordering::Acquire).as_mut().unwrap();
                for i in 0..per_thread {
                    let ptr = alloc.allocate();
                    write_volatile(ptr as *mut usize, i);
                    my_v.push(AtomicPtr::new(ptr));
                }
            }
            barrier.wait();
            time_block_once!(unsafe {
                for i in 0..per_thread {
                    let ptr = io_vec[them].load(Ordering::Acquire).as_ref().unwrap()[i]
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
    println!("{} Mops/s",
             ((nthreads * nthreads * per_thread * 1_000) as f64) / (total as f64));
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
    println!("{} Mops/s",
             ((nthreads * nthreads * per_thread * 2 * 1_000) as f64) / (total as f64));
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
    println!("{} Mops/s",
             ((nthreads * nthreads * per_thread * 1_000) as f64) / (total as f64));
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
    println!("{} Mops/s",
             ((nthreads * nthreads * per_thread * 1_000) as f64) / (total as f64));
    a.kill()
}

macro_rules! run_bench_inner {
    ($bench:tt, $nthreads:expr, $iters:expr) => {
        let iters = $iters;
        let nthreads = $nthreads;
        println!("Slab allocator (serial)");
        $bench::<SerialSlabAlloc<BenchItem>>(nthreads, iters);
        println!("current global allocator");
        $bench::<DefaultMalloc<BenchItem>>(nthreads, iters);
        println!("global slag allocator");
        $bench::<ElfGlobal<BenchItem>>(nthreads, iters);
        println!("clone-based slag allocator");
        $bench::<ElfClone<BenchItem>>(nthreads, iters);
        println!("slag allocator");
        $bench::<LocalAllocator<BenchItem>>(nthreads, iters);
        println!("slagazine allocator");
        $bench::<MagazineAllocator<BenchItem>>(nthreads, iters);
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
    println!("allocating {} bytes per thread",
             ITERS * mem::size_of::<BenchItem>());

    run_bench!(both "alloc/free pairs", bench_alloc_free_pairs, nthreads, ITERS);
    run_bench!(both "buffered alloc/free pairs", bench_alloc_free_pairs_buffered, nthreads, ITERS);
    run_bench!(both "alloc (thread-local)", bench_alloc, nthreads, ITERS);
    run_bench!(both "free (thread-local)", bench_free, nthreads, ITERS);
    run_bench!(both "alloc & free (thread-local)", bench_alloc_free, nthreads, ITERS);
    run_bench!(threads "free (producer-consumer)", bench_prod_cons, nthreads, ITERS);
}


#[cfg(test)]
mod tests {
    use super::*;
    use test::Bencher;
    use std::ptr;

    unsafe fn bench_alloc_free_pairs<A: SerialAlloc>(b: &mut Bencher) {
        let mut a = A::create();
        b.iter(|| {
            let p = a.allocate();
            ptr::write_volatile(p as *mut u8, 0);
            a.deallocate(p);
        });
    }

    unsafe fn bench_buffered_alloc_free_pairs<A: SerialAlloc>(b: &mut Bencher) {
        const BUFFER_SIZE: usize = 1 << 20;
        let mut a = A::create();
        let mut vec = Vec::new();
        for _ in 0..BUFFER_SIZE {
            vec.push(a.allocate());
        }
        let mut i = 0;
        b.iter(|| {
            let idx = i % BUFFER_SIZE;
            let p = vec.get_unchecked_mut(idx);
            a.deallocate(*p);
            ptr::write_volatile(*p as *mut u8, i as u8);
            *p = a.allocate();
            i += 1;
        });

        while let Some(p) = vec.pop() {
            a.deallocate(p);
        }
    }

    unsafe fn bench_alloc<A: SerialAlloc>(b: &mut Bencher) {
        let mut a = A::create();
        let mut vec = Vec::with_capacity(2 << 20);
        b.iter(|| {
            let p = a.allocate();
            ptr::write_volatile(p as *mut u8, 0);
            vec.push(p);
        });
        while let Some(p) = vec.pop() {
            a.deallocate(p);
        }
    }

    // Currently can't get this to work given the current bencher API. :(
    // This code compiles, but returns all 0s because frees past N_ITERS take no time and the
    // algorithm converges on 0 time taken overall.
    // unsafe fn bench_free<A: SerialAlloc>(b: &mut Bencher) {
    //     const N_ITERS: u64 = 4 << 20;
    //     let mut a = A::create();
    //     let mut vec = Vec::with_capacity(N_ITERS as usize);
    //     for _ in 0..N_ITERS {
    //         let p = a.allocate();
    //         ptr::write_volatile(p as *mut u8, 0);
    //         vec.push(p)
    //     }
    //     let mut i = 0;
    //     b.iter(|| {
    //         if i == N_ITERS {
    //             return;
    //         }
    //         a.deallocate(*vec.get_unchecked_mut(i as usize));
    //         i += 1;
    //     });
    // }

    // #[bench]
    // fn bench_free_default(b: &mut Bencher) {
    //     unsafe { bench_free::<DefaultMalloc<BenchItem>>(b) }
    // }

    // #[bench]
    // fn bench_free_slab(b: &mut Bencher) {
    //     unsafe { bench_free::<SerialSlabAlloc<BenchItem>>(b) }
    // }

    // #[bench]
    // fn bench_free_elf_global(b: &mut Bencher) {
    //     unsafe { bench_free::<ElfGlobal<BenchItem>>(b) }
    // }

    // #[bench]
    // fn bench_free_elf_clone(b: &mut Bencher) {
    //     unsafe { bench_free::<ElfClone<BenchItem>>(b) }
    // }

    // #[bench]
    // fn bench_free_local_slag(b: &mut Bencher) {
    //     unsafe { bench_free::<LocalAllocator<BenchItem>>(b) }
    // }

    // #[bench]
    // fn bench_free_magazine_slag(b: &mut Bencher) {
    //     unsafe { bench_free::<MagazineAllocator<BenchItem>>(b) }
    // }


    #[bench]
    fn bench_alloc_default(b: &mut Bencher) {
        unsafe { bench_alloc::<DefaultMalloc<BenchItem>>(b) }
    }

    #[bench]
    fn bench_alloc_slab(b: &mut Bencher) {
        unsafe { bench_alloc::<SerialSlabAlloc<BenchItem>>(b) }
    }

    // currently seg-faults due to heap overflow
    // #[bench]
    // fn bench_alloc_elf_global(b: &mut Bencher) {
    //     unsafe { bench_alloc::<ElfGlobal<BenchItem>>(b) }
    // }

    #[bench]
    fn bench_alloc_elf_clone(b: &mut Bencher) {
        unsafe { bench_alloc::<ElfClone<BenchItem>>(b) }
    }

    #[bench]
    fn bench_alloc_local_slag(b: &mut Bencher) {
        unsafe { bench_alloc::<LocalAllocator<BenchItem>>(b) }
    }

    #[bench]
    fn bench_alloc_magazine_slag(b: &mut Bencher) {
        unsafe { bench_alloc::<MagazineAllocator<BenchItem>>(b) }
    }

    #[bench]
    fn bench_alloc_free_default(b: &mut Bencher) {
        unsafe { bench_alloc_free_pairs::<DefaultMalloc<BenchItem>>(b) }
    }

    #[bench]
    fn bench_alloc_free_slab(b: &mut Bencher) {
        unsafe { bench_alloc_free_pairs::<SerialSlabAlloc<BenchItem>>(b) }
    }

    #[bench]
    fn bench_alloc_free_elf_global(b: &mut Bencher) {
        unsafe { bench_alloc_free_pairs::<ElfGlobal<BenchItem>>(b) }
    }

    #[bench]
    fn bench_alloc_free_elf_clone(b: &mut Bencher) {
        unsafe { bench_alloc_free_pairs::<ElfClone<BenchItem>>(b) }
    }

    #[bench]
    fn bench_alloc_free_local_slag(b: &mut Bencher) {
        unsafe { bench_alloc_free_pairs::<LocalAllocator<BenchItem>>(b) }
    }

    #[bench]
    fn bench_alloc_free_magazine_slag(b: &mut Bencher) {
        unsafe { bench_alloc_free_pairs::<MagazineAllocator<BenchItem>>(b) }
    }

    #[bench]
    fn bench_buffered_alloc_free_default(b: &mut Bencher) {
        unsafe { bench_buffered_alloc_free_pairs::<DefaultMalloc<BenchItem>>(b) }
    }

    #[bench]
    fn bench_buffered_alloc_free_slab(b: &mut Bencher) {
        unsafe { bench_buffered_alloc_free_pairs::<SerialSlabAlloc<BenchItem>>(b) }
    }

    #[bench]
    fn bench_buffered_alloc_free_elf_global(b: &mut Bencher) {
        unsafe { bench_buffered_alloc_free_pairs::<ElfGlobal<BenchItem>>(b) }
    }

    #[bench]
    fn bench_buffered_alloc_free_elf_clone(b: &mut Bencher) {
        unsafe { bench_buffered_alloc_free_pairs::<ElfClone<BenchItem>>(b) }
    }

    #[bench]
    fn bench_buffered_alloc_free_local_slag(b: &mut Bencher) {
        unsafe { bench_buffered_alloc_free_pairs::<LocalAllocator<BenchItem>>(b) }
    }

    #[bench]
    fn bench_buffered_alloc_free_magazine_slag(b: &mut Bencher) {
        unsafe { bench_buffered_alloc_free_pairs::<MagazineAllocator<BenchItem>>(b) }
    }



}
