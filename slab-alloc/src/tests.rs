// Copyright 2017 the authors. See the 'Copyright and license' section of the
// README.md file at the top-level directory of this repository.
//
// Licensed under the Apache License, Version 2.0 (the LICENSE-APACHE file) or
// the MIT license (the LICENSE-MIT file) at your option. This file may not be
// copied, modified, or distributed except according to those terms.

extern crate alloc;
extern crate object_alloc;
extern crate object_alloc_test;
extern crate rand;
extern crate sysconf;
extern crate test;

use SlabAllocBuilder;
use self::alloc::heap::{Alloc, Heap, Layout};
use self::object_alloc::{Exhausted, ObjectAlloc};
use self::test::{Bencher, black_box};
use self::object_alloc_test::leaky_alloc::LeakyAlloc;
use backing::alloc::AllocObjectAlloc;
use backing::BackingAlloc;
use SlabAlloc;

fn infer_allocator_type<T>(alloc: &mut ObjectAlloc<T>) {
    if false {
        let _: Result<*mut T, Exhausted> = unsafe { alloc.alloc() };
    }
}

struct LeakyBackingAlloc;

impl BackingAlloc for LeakyBackingAlloc {
    type Aligned = AllocObjectAlloc<LeakyAlloc>;
    type Large = AllocObjectAlloc<LeakyAlloc>;
}

fn leaky_get_aligned(layout: Layout) -> Option<AllocObjectAlloc<LeakyAlloc>> {
    if layout.align() == self::sysconf::page::pagesize() {
        Some(AllocObjectAlloc::new(LeakyAlloc::new(), layout))
    } else {
        None
    }
}

fn leaky_get_large(layout: Layout) -> AllocObjectAlloc<LeakyAlloc> {
    AllocObjectAlloc::new(LeakyAlloc::new(), layout)
}

fn test_memory_corruption<T: Copy + Send + 'static>() {
    use self::object_alloc_test::foreach_align;
    use self::object_alloc_test::corruption::{CorruptionTesterDefault, TestBuilder};
    use std::env;
    let default = 100_000;
    let iters = match env::var("SLAB_TEST_ITERS") {
        Ok(val) => val.parse().unwrap_or(default),
        Err(_) => default,
    };
    let f = |align| {
        let new = move || {
            SlabAllocBuilder::default()
                .align(align)
                .build_backing(leaky_get_aligned, leaky_get_large) as
            SlabAlloc<_, _, LeakyBackingAlloc>
        };
        infer_allocator_type::<CorruptionTesterDefault<T>>(&mut new());
        TestBuilder::new(new).test_iters(iters).test();
    };
    foreach_align::<CorruptionTesterDefault<T>, _>(f, self::sysconf::page::pagesize());
}

fn test_quickcheck_memory_corruption<T: Copy + Send + 'static>() {
    use self::object_alloc_test::foreach_align;
    use self::object_alloc_test::corruption::{CorruptionTesterDefault, TestBuilder};
    use std::env;
    let default = 100_000;
    let tests = match env::var("SLAB_QUICKCHECK_TESTS") {
        Ok(val) => val.parse().unwrap_or(default),
        Err(_) => default,
    };
    let f = |align| {
        let new = move || {
            SlabAllocBuilder::default()
                .align(align)
                .build_backing(leaky_get_aligned, leaky_get_large) as
            SlabAlloc<_, _, LeakyBackingAlloc>
        };
        infer_allocator_type::<CorruptionTesterDefault<T>>(&mut new());
        TestBuilder::new(new)
            .quickcheck_tests(tests)
            .quickcheck();
    };
    foreach_align::<CorruptionTesterDefault<T>, _>(f, self::sysconf::page::pagesize());
}

macro_rules! make_test_memory_corruption {
    // The corruption checker can't handle types smaller than 9 bytes. It's easier to turn these
    // into noops and still call define_for_all_types_prefix! than to manually define all of the
    // tests that we /do/ want to define.
    ($name:ident, $crate::types::Byte1) => ();
    ($name:ident, $crate::types::Byte2) => ();
    ($name:ident, $crate::types::Byte3) => ();
    ($name:ident, $crate::types::Byte4) => ();
    ($name:ident, $crate::types::Byte5) => ();
    ($name:ident, $crate::types::Byte6) => ();
    ($name:ident, $crate::types::Byte7) => ();
    ($name:ident, $crate::types::Byte8) => ();
    ($name:ident, $type:ty) => (
        #[test]
        fn $name() {
            test_memory_corruption::<$type>();
        }
    );
}

macro_rules! make_test_quickcheck_memory_corruption {
    // The corruption checker can't handle types smaller than 9 bytes. It's easier to turn these
    // into noops and still call define_for_all_types_prefix! than to manually define all of the
    // tests that we /do/ want to define.
    ($name:ident, $crate::types::Byte1) => ();
    ($name:ident, $crate::types::Byte2) => ();
    ($name:ident, $crate::types::Byte3) => ();
    ($name:ident, $crate::types::Byte4) => ();
    ($name:ident, $crate::types::Byte5) => ();
    ($name:ident, $crate::types::Byte6) => ();
    ($name:ident, $crate::types::Byte7) => ();
    ($name:ident, $crate::types::Byte8) => ();
    ($name:ident, $type:ty) => (
        #[test]
        #[ignore]
        fn $name() {
            test_quickcheck_memory_corruption::<$type>();
        }
    );
}

call_for_all_types_prefix!(make_test_memory_corruption, test_memory_corruption);
call_for_all_types_prefix!(make_test_quickcheck_memory_corruption,
                           quickcheck_memory_corruption);

#[cfg_attr(not(feature = "build-ignored-tests"), allow(unused))]
fn bench_alloc_no_free<T: Default>(b: &mut Bencher) {
    let mut alloc = SlabAllocBuilder::default().build();
    infer_allocator_type::<T>(&mut alloc);
    b.iter(|| unsafe { black_box(alloc.alloc().unwrap()) });
    // since we didn't free anything, dropping alloc would result in a refcnt check failing
    use std::mem;
    mem::forget(alloc);
}

#[cfg_attr(not(feature = "build-ignored-tests"), allow(unused))]
fn bench_alloc_no_free_no_init<T: Default>(b: &mut Bencher) {
    let mut alloc = unsafe { SlabAllocBuilder::no_initialize().build() };
    infer_allocator_type::<T>(&mut alloc);
    b.iter(|| unsafe { black_box(alloc.alloc().unwrap()) });
    // since we didn't free anything, dropping alloc would result in a refcnt check failing
    use std::mem;
    mem::forget(alloc);
}

#[cfg_attr(not(feature = "build-ignored-tests"), allow(unused))]
fn bench_alloc_no_free_heap<T: Default>(b: &mut Bencher) {
    let layout = Layout::new::<T>();
    b.iter(|| unsafe { black_box(Heap.alloc(layout.clone()).unwrap()) });
}

#[cfg_attr(not(feature = "build-ignored-tests"), allow(unused))]
fn bench_alloc_free_pairs<T: Default>(b: &mut Bencher) {
    let mut alloc = SlabAllocBuilder::default().build();
    infer_allocator_type::<T>(&mut alloc);
    // keep one allocated at all times so the slab will never be freed;
    // we're trying to bench the best-case performance, not the slab gc policy
    let t = unsafe { alloc.alloc().unwrap() };
    b.iter(|| unsafe {
               let t = alloc.alloc().unwrap();
               black_box(t);
               alloc.dealloc(t);
           });
    unsafe {
        alloc.dealloc(t);
    }
}

#[cfg_attr(not(feature = "build-ignored-tests"), allow(unused))]
fn bench_alloc_free_pairs_no_init<T: Default>(b: &mut Bencher) {
    let mut alloc = unsafe { SlabAllocBuilder::no_initialize().build() };
    infer_allocator_type::<T>(&mut alloc);
    // keep one allocated at all times so the slab will never be freed;
    // we're trying to bench the best-case performance, not the slab gc policy
    let t = unsafe { alloc.alloc().unwrap() };
    b.iter(|| unsafe {
               let t = alloc.alloc().unwrap();
               black_box(t);
               alloc.dealloc(t);
           });
    unsafe {
        alloc.dealloc(t);
    }
}

#[cfg_attr(not(feature = "build-ignored-tests"), allow(unused))]
fn bench_alloc_free_pairs_heap<T: Default>(b: &mut Bencher) {
    let layout = Layout::new::<T>();
    b.iter(|| unsafe {
               let t = Heap.alloc(layout.clone()).unwrap();
               black_box(t);
               Heap.dealloc(t, layout.clone());
           });
}

macro_rules! make_bench_alloc_no_free {
    ($name:ident, $typ:ty) => (
        #[bench]
        #[cfg(feature = "build-ignored-tests")]
        #[cfg_attr(not(feature = "build-ignored-tests"), allow(unused))]
        #[ignore]
        fn $name(b: &mut Bencher) { bench_alloc_no_free::<$typ>(b); }
    );
}
macro_rules! make_bench_alloc_no_free_no_init {
    ($name:ident, $typ:ty) => (
        #[bench]
        #[cfg(feature = "build-ignored-tests")]
        #[cfg_attr(not(feature = "build-ignored-tests"), allow(unused))]
        #[ignore]
        fn $name(b: &mut Bencher) { bench_alloc_no_free_no_init::<$typ>(b); }
    );
}
macro_rules! make_bench_alloc_no_free_heap {
    ($name:ident, $typ:ty) => (
        #[bench]
        #[cfg(feature = "build-ignored-tests")]
        #[cfg_attr(not(feature = "build-ignored-tests"), allow(unused))]
        #[ignore]
        fn $name(b: &mut Bencher) { bench_alloc_no_free_heap::<$typ>(b); }
    );
}
macro_rules! make_bench_alloc_free_pairs {
    ($name:ident, $typ:ty) => (
        #[bench]
        #[cfg(feature = "build-ignored-tests")]
        #[cfg_attr(not(feature = "build-ignored-tests"), allow(unused))]
        #[ignore]
        fn $name(b: &mut Bencher) { bench_alloc_free_pairs::<$typ>(b); }
    );
}
macro_rules! make_bench_alloc_free_pairs_no_init {
    ($name:ident, $typ:ty) => (
        #[bench]
        #[cfg(feature = "build-ignored-tests")]
        #[cfg_attr(not(feature = "build-ignored-tests"), allow(unused))]
        #[ignore]
        fn $name(b: &mut Bencher) { bench_alloc_free_pairs_no_init::<$typ>(b); }
    );
}
macro_rules! make_bench_alloc_free_pairs_heap {
    ($name:ident, $typ:ty) => (
        #[bench]
        #[cfg(feature = "build-ignored-tests")]
        #[cfg_attr(not(feature = "build-ignored-tests"), allow(unused))]
        #[ignore]
        fn $name(b: &mut Bencher) { bench_alloc_free_pairs_heap::<$typ>(b); }
    );
}

call_for_all_types_prefix!(make_bench_alloc_no_free, bench_alloc_no_free);
call_for_all_types_prefix!(make_bench_alloc_no_free_no_init,
                           bench_alloc_no_free_no_init);
call_for_all_types_prefix!(make_bench_alloc_no_free_heap, bench_alloc_no_free_heap);
call_for_all_types_prefix!(make_bench_alloc_free_pairs, bench_alloc_free_pairs);
call_for_all_types_prefix!(make_bench_alloc_free_pairs_no_init,
                           bench_alloc_free_pairs_no_init);
call_for_all_types_prefix!(make_bench_alloc_free_pairs_heap,
                           bench_alloc_free_pairs_heap);
