// Copyright 2017 the authors. See the 'Copyright and license' section of the
// README.md file at the top-level directory of this repository.
//
// Licensed under the Apache License, Version 2.0 (the LICENSE file). This file
// may not be copied, modified, or distributed except according to those terms.

//! An inital stab at `Alloc`-parametric collections.
//!
//! This module contains an `Alloc`-parametric `Vec` implementation based on `RawVec`.
//! This is currently more of a proof of concept, though it may serve as a starting point
//! for more robust `Alloc`-parametric collections.

extern crate smallvec;
use self::smallvec::VecLike;
use super::alloc::allocator::Alloc;
use super::alloc::heap::Heap;
use super::alloc::raw_vec::RawVec;
use super::rust_alloc;
use super::rust_alloc::{DynamicAlloc, SharedAlloc};

use std::cmp;
use std::iter::{IntoIterator, Extend};
use std::ops;
use std::ptr;

/// A `Vec`-like structure parametric on an `Alloc`. The overall structure here borrows heavily
/// from the smallvec crate, though our goals here are of course different. One could easily fork
/// smallvec to achieve a similar aim, but we want to focus on allocation in this setting and
/// smallvec is unlikely to help (our vectors will be large).
pub struct AVec<T, A: Alloc> {
    buf: RawVec<T, A>,
    len: usize,
}

impl<T, A: Alloc> VecLike<T> for AVec<T, A> {
    #[inline]
    fn push(&mut self, val: T) {
        // borrowed from the RawVec docs
        if self.len == self.buf.cap() {
            self.buf.double();
        }
        unsafe {
            ptr::write(self.buf.ptr().offset(self.len as isize), val);
        }
        self.len += 1;
    }
}

impl<T, A: Alloc> AVec<T, A>
where
    Self: Default,
{
    pub fn new() -> Self {
        Self::default()
    }

    pub fn with_capacity(cap: usize) -> Self {
        let mut res = Self::new();
        res.reserve(cap);
        res
    }
}

impl<T2, T1: PartialEq<T2>, A1: Alloc, A2: Alloc> PartialEq<AVec<T2, A2>> for AVec<T1, A1> {
    #[inline]
    fn eq(&self, other: &AVec<T2, A2>) -> bool {
        self[..] == other[..]
    }

    #[inline]
    fn ne(&self, other: &AVec<T2, A2>) -> bool {
        self[..] != other[..]
    }
}

impl<T: Eq, A: Alloc> Eq for AVec<T, A> {}

impl<T: PartialOrd, A: Alloc> PartialOrd for AVec<T, A> {
    #[inline]
    fn partial_cmp(&self, other: &AVec<T, A>) -> Option<cmp::Ordering> {
        PartialOrd::partial_cmp(&**self, &**other)
    }
}

impl<T: Ord, A: Alloc> Ord for AVec<T, A> {
    #[inline]
    fn cmp(&self, other: &AVec<T, A>) -> cmp::Ordering {
        Ord::cmp(&**self, &**other)
    }
}

impl<T> Default for AVec<T, DynamicAlloc> {
    fn default() -> AVec<T, DynamicAlloc> {
        AVec {
            buf: RawVec::new_in(rust_alloc::new_owned_handle()),
            len: 0,
        }
    }
}

impl<T> Default for AVec<T, SharedAlloc> {
    fn default() -> AVec<T, SharedAlloc> {
        AVec {
            buf: RawVec::new_in(SharedAlloc),
            len: 0,
        }
    }
}

impl<T> Default for AVec<T, Heap> {
    fn default() -> AVec<T, Heap> {
        AVec {
            buf: RawVec::new(),
            len: 0,
        }
    }
}

impl<T, A: Alloc> Drop for AVec<T, A> {
    fn drop(&mut self) {
        for i in 0..(self.len as isize) {
            unsafe {
                ptr::drop_in_place(self.buf.ptr().offset(i));
            }
        }
    }
}

impl<T, A: Alloc> AVec<T, A> {
    pub fn pop(&mut self) -> Option<T> {
        if self.len == 0 {
            None
        } else {
            self.len -= 1;
            unsafe { Some(ptr::read(self.buf.ptr().offset(self.len as isize))) }
        }
    }

    pub fn reserve(&mut self, extra_bytes: usize) {
        self.buf.reserve(self.len, extra_bytes);
    }

    pub fn resize(&mut self, new_cap: usize) {
        if new_cap == self.len {
            return;
        }
        if new_cap > self.len {
            let extra_bytes = new_cap - self.len;
            self.reserve(extra_bytes);
            return;
        }

        while self.len > new_cap {
            self.len -= 1;
            unsafe {
                ptr::drop_in_place(self.buf.ptr().offset(self.len as isize));
            }
        }
        self.buf.shrink_to_fit(new_cap);
    }

    unsafe fn get_raw(&self, ix: usize) -> *mut T {
        self.buf.ptr().offset(ix as isize)
    }

    unsafe fn to_slice(&self) -> *mut [T] {
        ::std::slice::from_raw_parts_mut(self.buf.ptr(), self.len)
    }
}

macro_rules! forward_slice_index_impl {
    ($input:ty, $output:ty) => {

        impl<T, A: Alloc> ops::Index<$input> for AVec<T, A> {
            type Output = $output;
            fn index(&self, ix: $input) -> &$output {
                (&**self).index(ix)
            }
        }

        impl<T, A: Alloc> ops::IndexMut<$input> for AVec<T, A> {
            fn index_mut(&mut self, ix: $input) -> &mut $output {
                (&mut**self).index_mut(ix)
            }
        }
    };
}

forward_slice_index_impl!(ops::Range<usize>, [T]);
forward_slice_index_impl!(ops::RangeFrom<usize>, [T]);
forward_slice_index_impl!(ops::RangeTo<usize>, [T]);
forward_slice_index_impl!(ops::RangeFull, [T]);

impl<T, A: Alloc> ops::Index<usize> for AVec<T, A> {
    type Output = T;
    fn index(&self, ix: usize) -> &T {
        assert!(ix < self.len);
        unsafe { &*self.get_raw(ix) }
    }
}

impl<T, A: Alloc> ops::IndexMut<usize> for AVec<T, A> {
    fn index_mut(&mut self, ix: usize) -> &mut T {
        assert!(ix < self.len);
        unsafe { &mut *self.get_raw(ix) }
    }
}

impl<T, A: Alloc> Extend<T> for AVec<T, A> {
    fn extend<I: IntoIterator<Item = T>>(&mut self, iterable: I) {
        let iter = iterable.into_iter();
        let (lower_bound, _) = iter.size_hint();
        self.buf.reserve_exact(self.len, lower_bound);
        for item in iter {
            self.push(item);
        }
    }
}

impl<T, A: Alloc> ops::Deref for AVec<T, A> {
    type Target = [T];
    fn deref(&self) -> &[T] {
        unsafe { &*self.to_slice() }
    }
}

impl<T, A: Alloc> ops::DerefMut for AVec<T, A> {
    fn deref_mut(&mut self) -> &mut [T] {
        unsafe { &mut *self.to_slice() }
    }
}


#[cfg(test)]
mod tests {
    extern crate env_logger;
    extern crate test;
    use self::test::Bencher;

    use super::*;
    type RVec<T> = AVec<T, SharedAlloc>;

    #[test]
    fn test_many_pushes() {
        let _ = env_logger::init();
        let mut rv = RVec::new();
        for i in 0..1000 {
            rv.push(i);
        }
        let expect: Vec<_> = (0..1000).collect();
        assert_eq!(&*rv, &expect[..]);
    }

    #[test]
    fn test_pushes_pops() {
        let _ = env_logger::init();
        let mut rv = RVec::new();
        for i in 0..1000 {
            rv.push(i);
        }
        let _ = rv.pop();
        rv.push(!0);
        let mut expect: Vec<_> = (0..999).collect();
        expect.push(!0);
        assert_eq!(&*rv, &expect[..]);
    }

    #[test]
    fn test_extend() {
        let _ = env_logger::init();
        let mut rv = RVec::new();
        for i in 0..1000 {
            rv.push(i);
        }
        let mut expect: Vec<_> = (0..1000).collect();
        let tmp = expect.clone();
        expect.extend(&*rv);
        rv.extend(tmp.into_iter());
        assert_eq!(&*rv, &expect[..]);
    }

    #[bench]
    fn bench_push_avec_elf(b: &mut Bencher) {
        bench_push::<AVec<usize, DynamicAlloc>>(b);
    }

    #[bench]
    fn bench_push_avec_shared_elf(b: &mut Bencher) {
        bench_push::<AVec<usize, SharedAlloc>>(b);
    }

    #[bench]
    fn bench_push_avec_heap(b: &mut Bencher) {
        bench_push::<AVec<usize, Heap>>(b);
    }

    #[bench]
    fn bench_push_vec(b: &mut Bencher) {
        bench_push::<Vec<usize>>(b);
    }

    fn bench_push<V: VecLike<usize> + Default>(b: &mut Bencher) {
        #[inline(never)]
        fn push_noinline<T, V: VecLike<T>>(vec: &mut V, t: T) {
            vec.push(t);
        }
        b.iter(|| {
            let mut vec = V::default();
            for x in 0..(1 << 10) {
                push_noinline(&mut vec, x);
            }
            test::black_box(vec)
        });
    }

    #[bench]
    fn bench_push_nested_avec_elf(b: &mut Bencher) {
        bench_push_nested::<
            AVec<usize, DynamicAlloc>,
            AVec<AVec<usize, DynamicAlloc>, DynamicAlloc>,
        >(b);
    }

    #[bench]
    fn bench_push_nested_avec_shared_elf(b: &mut Bencher) {
        bench_push_nested::<AVec<usize, SharedAlloc>,
                            AVec<AVec<usize, SharedAlloc>, SharedAlloc>>(b);
    }

    #[bench]
    fn bench_push_nested_avec_heap(b: &mut Bencher) {
        bench_push_nested::<AVec<usize, Heap>, AVec<AVec<usize, Heap>, Heap>>(b);
    }

    #[bench]
    fn bench_push_nested_vec(b: &mut Bencher) {
        bench_push_nested::<Vec<usize>, Vec<Vec<usize>>>(b);
    }

    fn bench_push_nested<V: VecLike<usize> + Default, VV: VecLike<V> + Default>(b: &mut Bencher) {
        #[inline(never)]
        fn push_noinline<T, V2: VecLike<T>>(vec: &mut V2, t: T) {
            vec.push(t);
        }
        b.iter(|| {
            let mut big_vec = VV::default();
            for _ in 0..128 {
                let mut vec = V::default();
                for x in 0..(1 << 10) {
                    push_noinline(&mut vec, x);
                }
                push_noinline(&mut big_vec, vec);
            }
            test::black_box(big_vec)
        });
    }

    #[bench]
    fn bench_extend_avec_elf(b: &mut Bencher) {
        bench_extend::<AVec<usize, DynamicAlloc>>(b);
    }

    #[bench]
    fn bench_extend_avec_shared_elf(b: &mut Bencher) {
        bench_extend::<AVec<usize, SharedAlloc>>(b);
    }

    #[bench]
    fn bench_extend_avec_heap(b: &mut Bencher) {
        bench_extend::<AVec<usize, Heap>>(b);
    }

    #[bench]
    fn bench_extend_vec(b: &mut Bencher) {
        bench_extend::<Vec<usize>>(b);
    }

    fn bench_extend<V: VecLike<usize> + Default>(b: &mut Bencher) {
        #[inline(never)]
        fn extend_noinline<V: VecLike<usize>>(vec: &mut V) {
            vec.extend((0..(1 << 10)));
        }
        b.iter(|| {
            let mut vec = V::default();
            for _ in 0..10 {
                extend_noinline(&mut vec);
            }
            test::black_box(vec)
        });
    }
}
