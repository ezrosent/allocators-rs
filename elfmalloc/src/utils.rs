// Copyright 2017 the authors. See the 'Copyright and license' section of the
// README.md file at the top-level directory of this repository.
//
// Licensed under the Apache License, Version 2.0 (the LICENSE-APACHE file) or
// the MIT license (the LICENSE-MIT file) at your option. This file may not be
// copied, modified, or distributed except according to those terms.

//! Some basic utilities used throughout the allocator code.
use std::cmp;
use std::ops::{Deref, DerefMut};
use std::cell::UnsafeCell;

pub mod mmap {
    extern crate mmap_alloc;
    extern crate sysconf;
    use self::mmap_alloc::MapAllocBuilder;
    use super::super::alloc::allocator::{Alloc, Layout};

    pub fn page_size() -> usize {
        self::sysconf::page::pagesize()
    }

    pub fn map(size: usize) -> *mut u8 {
        fallible_map(size).expect("mmap should not fail")
    }

    pub fn fallible_map(size: usize) -> Option<*mut u8> {
        unsafe {
            if let Ok(s) = MapAllocBuilder::default()
                   .exec()
                   .build()
                   .alloc(Layout::from_size_align(size, 1).unwrap()) {
                Some(s)
            } else {
                None
            }
        }
    }

    pub unsafe fn unmap(p: *mut u8, len: usize) {
        MapAllocBuilder::default().exec().build().dealloc(
            p,
            Layout::from_size_align(len, 1).unwrap(),
        )
    }
    pub unsafe fn uncommit(p: *mut u8, len: usize) {
        MapAllocBuilder::default().exec().build().uncommit(
            p,
            Layout::from_size_align(len, 1).unwrap(),
        )
    }
}

// we use the unlikely intrinsic if it is available.

#[cfg(feature = "nightly")]
pub use std::intrinsics::{likely, unlikely};

#[cfg(not(feature = "nightly"))]
#[cfg_attr(feature = "cargo-clippy", allow(inline_always))]
#[inline(always)]
unsafe fn unlikely(b: bool) -> bool {
    b
}

#[cfg(not(feature = "nightly"))]
#[cfg_attr(feature = "cargo-clippy", allow(inline_always))]
#[inline(always)]
unsafe fn likely(b: bool) -> bool {
    b
}

/// A `LazyInitializable` type can be constructed from `Params`.
///
/// Types that implement this trate can be wrapped in the `Lazy` construct.
pub trait LazyInitializable {
    type Params;
    fn init(p: &Self::Params) -> Self;
}

/// A `Lazy` instance of a type `T` keeps `T::Params` strict but only initializes the value with
/// `T::init` when it is first accessed.
///
/// `Lazy` implements `Clone` if `T::Params` does. Note that this only clones the constructor
/// parameters and not the object itself. These semantics are appropriate for their use in
/// allocator size classes, but may be unintuitive elsewhere. It implements `deref` and `deref_mut`
/// to facilitate access to the underlying object.
pub struct Lazy<T: LazyInitializable> {
    params: T::Params,
    val: UnsafeCell<Option<T>>,
}

impl<T: LazyInitializable> Clone for Lazy<T>
where
    T::Params: Clone,
{
    fn clone(&self) -> Self {
        Lazy {
            params: self.params.clone(),
            val: UnsafeCell::new(None),
        }
    }
}

impl<T: LazyInitializable> Lazy<T> {
    /// Create a new `Lazy<T>` with constructor parameters given by `params`.
    pub fn new(params: T::Params) -> Self {
        Lazy {
            params: params,
            val: UnsafeCell::new(None),
        }
    }
}

impl<T: LazyInitializable> Deref for Lazy<T> {
    type Target = T;

    #[cfg_attr(feature = "cargo-clippy", allow(inline_always))]
    #[inline(always)]
    fn deref(&self) -> &T {
        let state = unsafe { &mut *self.val.get() };
        if unsafe { unlikely(state.is_none()) } {
            *state = Some(T::init(&self.params));
        }
        state.as_ref().unwrap()
    }
}

impl<T: LazyInitializable> DerefMut for Lazy<T> {
    #[cfg_attr(feature = "cargo-clippy", allow(inline_always))]
    #[inline(always)]
    fn deref_mut(&mut self) -> &mut T {
        let state = unsafe { &mut *self.val.get() };
        if unsafe { unlikely(state.is_none()) } {
            *state = Some(T::init(&self.params));
        }
        state.as_mut().unwrap()
    }
}


/// A low-level dynamic collection of `T` values.
///
/// `TypedArray` uses mmap for memory allocation. This means that memory consumption from a
/// `TypedArray` is lazy: the pages are only backed by physical memory after they are used. A
/// `TypedArray` does not free its memory in a destructor (these semantics are required for the
/// global allocator in the `general` module). To reclaim the memory used by the array, a `destroy`
/// method is supplied.
///
/// To use a `TypedArray` with a more traditional RAII-style destructor, use `OwnedArray`.
pub struct TypedArray<T> {
    // TODO: replace with non-null once that stabilizes.
    data: *mut T,
    len: usize,
    mapped: usize,
}

pub const PAGE_SIZE: usize = 4096;

impl<T> TypedArray<T> {
    pub fn new(size: usize) -> TypedArray<T> {
        use std::mem::size_of;
        let bytes = size_of::<T>() * size;
        let rem = bytes % PAGE_SIZE;
        let n_pages = bytes / PAGE_SIZE + cmp::min(1, rem);
        let region_size = n_pages * PAGE_SIZE;
        let mem = mmap::map(region_size);
        TypedArray {
            data: mem as *mut T,
            len: size,
            mapped: region_size,
        }
    }

    pub fn iter(&self) -> TypedArrayIter<T> {
        TypedArrayIter {
            inner: self,
            cur: 0,
        }
    }

    /// Get an index into the array. Unsafe because this operation is unchecked: it may provide a
    /// pointer out of bounds.
    pub unsafe fn get(&self, n: usize) -> *mut T {
        self.data.offset(n as isize)
    }

    pub fn len(&self) -> usize {
        self.len
    }

    pub unsafe fn destroy(&self) {
        mmap::unmap(self.data as *mut u8, self.mapped);
    }
}

/// A variant of `TypedArray` that unmaps its memory during `drop`.
pub struct OwnedArray<T>(TypedArray<T>);

impl<T> OwnedArray<T> {
    pub fn new(size: usize) -> OwnedArray<T> {
        OwnedArray(TypedArray::new(size))
    }
}

impl<T> Deref for OwnedArray<T> {
    type Target = TypedArray<T>;
    fn deref(&self) -> &TypedArray<T> {
        &self.0
    }
}

impl<T> Drop for OwnedArray<T> {
    fn drop(&mut self) {
        unsafe { self.destroy() }
    }
}

pub struct TypedArrayIter<'a, T: 'a> {
    inner: &'a TypedArray<T>,
    cur: usize,
}

impl<'a, T: 'a> Iterator for TypedArrayIter<'a, T> {
    type Item = *mut T;
    fn next(&mut self) -> Option<*mut T> {
        if self.cur == self.inner.len {
            None
        } else {
            unsafe {
                let res = self.inner.data.offset(self.cur as isize);
                self.cur += 1;
                Some(res)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    #[derive(Debug)]
    struct DefaultInit<T: Default>(T);
    impl<T: Default> LazyInitializable for DefaultInit<T> {
        type Params = ();
        fn init(_p: &()) -> Self {
            DefaultInit(T::default())
        }
    }

    use super::*;
    #[test]
    fn basic_functionality() {
        let mut l = Lazy::<DefaultInit<usize>>::new(());
        let l_u = l.0;
        assert_eq!(l_u, 0);
        *l = DefaultInit(1);
        let l_u = l.0;
        assert_eq!(l_u, 1);
    }

}
