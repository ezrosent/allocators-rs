extern crate alloc;
extern crate object_alloc;
extern crate object_alloc_test;
extern crate rand;
extern crate sysconf;
extern crate test;

use SlabAllocBuilder;
use self::alloc::allocator::Layout;
use self::object_alloc::{Exhausted, ObjectAlloc};
use self::test::Bencher;
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
    if layout.align() == self::sysconf::pagesize() {
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
    foreach_align::<CorruptionTesterDefault<T>, _>(f, self::sysconf::pagesize());
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
    foreach_align::<CorruptionTesterDefault<T>, _>(f, self::sysconf::pagesize());
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
    // test::black_box isn't necessary here; alloc.alloc won't be optimized away.
    // However, we keep it in order to be fair to bench_alloc_no_free_system, which
    // has to use it in order to avoid the call to Box::new being optimized away.
    b.iter(|| test::black_box(unsafe { alloc.alloc() }));
    // since we didn't free anything, dropping alloc would result in a refcnt check failing
    use std::mem;
    mem::forget(alloc);
}

#[cfg_attr(not(feature = "build-ignored-tests"), allow(unused))]
fn bench_alloc_no_free_system<T: Default>(b: &mut Bencher) {
    use std::mem;
    // use test::black_box so that StdBox::new isn't optimized away
    b.iter(|| mem::forget(test::black_box(Box::new(T::default()))));
}

#[cfg_attr(not(feature = "build-ignored-tests"), allow(unused))]
fn bench_alloc_free<T: Default>(b: &mut Bencher) {
    let mut alloc = SlabAllocBuilder::default().build();
    infer_allocator_type::<T>(&mut alloc);
    // keep one allocated at all times so the slab will never be freed;
    // we're trying to bench the best-case performance, not the slab gc policy
    let t = unsafe { alloc.alloc().unwrap() };
    b.iter(|| unsafe {
               let t = alloc.alloc().unwrap();
               alloc.dealloc(t);
           });
    unsafe {
        alloc.dealloc(t);
    }
}

#[cfg_attr(not(feature = "build-ignored-tests"), allow(unused))]
fn bench_alloc_free_box<T: Default>(b: &mut Bencher) {
    let mut alloc = SlabAllocBuilder::default().build();
    infer_allocator_type::<T>(&mut alloc);
    // keep one allocated at all times so the slab will never be freed;
    // we're trying to bench the best-case performance, not the slab gc policy
    let t = unsafe { alloc.alloc().unwrap() };
    b.iter(|| Box::new(&alloc));
    unsafe {
        alloc.dealloc(t);
    }
}

#[cfg_attr(not(feature = "build-ignored-tests"), allow(unused))]
fn bench_alloc_free_system<T: Default>(b: &mut Bencher) {
    b.iter(|| Box::new(T::default()));
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
macro_rules! make_bench_alloc_no_free_system {
    ($name:ident, $typ:ty) => (
        #[bench]
        #[cfg(feature = "build-ignored-tests")]
        #[cfg_attr(not(feature = "build-ignored-tests"), allow(unused))]
        #[ignore]
        fn $name(b: &mut Bencher) { bench_alloc_no_free_system::<$typ>(b); }
    );
}
macro_rules! make_bench_alloc_free {
    ($name:ident, $typ:ty) => (
        #[bench]
        #[cfg(feature = "build-ignored-tests")]
        #[cfg_attr(not(feature = "build-ignored-tests"), allow(unused))]
        #[ignore]
        fn $name(b: &mut Bencher) { bench_alloc_free::<$typ>(b); }
    );
}
macro_rules! make_bench_alloc_free_box {
    ($name:ident, $typ:ty) => (
        #[bench]
        #[cfg(feature = "build-ignored-tests")]
        #[cfg_attr(not(feature = "build-ignored-tests"), allow(unused))]
        #[ignore]
        fn $name(b: &mut Bencher) { bench_alloc_free_box::<$typ>(b); }
    );
}
macro_rules! make_bench_alloc_free_system {
    ($name:ident, $typ:ty) => (
        #[bench]
        #[cfg(feature = "build-ignored-tests")]
        #[cfg_attr(not(feature = "build-ignored-tests"), allow(unused))]
        #[ignore]
        fn $name(b: &mut Bencher) { bench_alloc_free_system::<$typ>(b); }
    );
}

call_for_all_types_prefix!(make_bench_alloc_no_free, bench_alloc_no_free);
call_for_all_types_prefix!(make_bench_alloc_no_free_system, bench_alloc_no_free_system);
call_for_all_types_prefix!(make_bench_alloc_free, bench_alloc_free);
call_for_all_types_prefix!(make_bench_alloc_free_box, bench_alloc_free_box);
call_for_all_types_prefix!(make_bench_alloc_free_system, bench_alloc_free_system);
