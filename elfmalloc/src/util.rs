// Copyright 2017-2018 the authors. See the 'Copyright and license' section of the
// README.md file at the top-level directory of this repository.
//
// Licensed under the Apache License, Version 2.0 (the LICENSE-APACHE file) or
// the MIT license (the LICENSE-MIT file) at your option. This file may not be
// copied, modified, or distributed except according to those terms.

//! Some basic utilities used throughout the allocator code.
use std::cell::UnsafeCell;
use std::cmp;
use std::intrinsics::unlikely;
use std::ops::{Deref, DerefMut};
use std::ptr::NonNull;

use alloc::raw_vec::RawVec;
use crossbeam_epoch::Handle;
use frontends::Frontend;
use object_alloc::UntypedObjectAlloc;

pub mod mmap {
    use mmap_alloc::{MapAlloc, MapAllocBuilder};
    use alloc::allocator::{Alloc, Layout};

    lazy_static!{ 
        static ref MMAP: MapAlloc = MapAllocBuilder::default()
            .commit(cfg!(windows))
            .build();
    }

    pub fn page_size() -> usize {
        ::sysconf::page::pagesize()
    }

    pub fn map(size: usize) -> *mut u8 {
        fallible_map(size).expect("mmap should not fail")
    }

    pub fn fallible_map(size: usize) -> Option<*mut u8> {
        unsafe { (&*MMAP).alloc(layout_for_size(size)).ok() }
    }

    pub unsafe fn unmap(p: *mut u8, size: usize) {
        (&*MMAP).dealloc(p, layout_for_size(size));
    }

    pub unsafe fn commit(p: *mut u8, size: usize) {
        (&*MMAP).commit(p, layout_for_size(size))
    }

    pub unsafe fn uncommit(p: *mut u8, size: usize) {
        (&*MMAP).uncommit(p, layout_for_size(size));
    }

    fn layout_for_size(size: usize) -> Layout {
        Layout::from_size_align(size, page_size()).unwrap()
     }
}

/// A type which can fallibly clone itself.
/// 
/// `TryClone::clone` is like `Clone::clone`, except that it will return `None`
/// if allocation failed while performing the clone operation.
pub trait TryClone
where
    Self: Sized
{
    fn try_clone(&self) -> Option<Self>;
}

impl<T: Clone> TryClone for T {
    fn try_clone(&self) -> Option<Self> {
        Some(self.clone())
    }
}

/// An allocator which requires the caller to provide extra information.
/// 
/// An `AllocWith` is an allocator which requires the caller to provide a
/// `Handle` to a Crossbeam epoch GC instance and an allocator for backing
/// pages.
pub unsafe trait AllocWith {
    unsafe fn alloc_with<P: UntypedObjectAlloc>(&mut self, handle: &Handle, pages: &mut P) -> Option<NonNull<u8>>;
    unsafe fn dealloc_with<P: UntypedObjectAlloc>(&mut self, handle: &Handle, pages: &mut P, ptr: NonNull<u8>);
    unsafe fn pre_drop<P: UntypedObjectAlloc>(&mut self, handle: &Handle, pages: &mut P);
}

pub trait TryCloneWith
where Self: Sized
{
    fn try_clone_with<P: UntypedObjectAlloc>(&self, handle: &Handle, pages: &mut P) -> Option<Self>;
}

pub trait Const<T> {
    const VAL: T;
}

/// A `LazyInitializable` type can be constructed from `Params`.
///
/// Types that implement this trait can be wrapped in the `Lazy` construct.
pub trait LazyInitializable
where Self: Sized
{
    type Params;

    fn init<P: UntypedObjectAlloc>(stored: &Self::Params, handle: &Handle, pages: &mut P) -> Option<Self>;
}

/// A `Lazy` instance of a type `T` keeps `T::Params` strict but only
/// initializes the value with `T::init` when it is first accessed.
///
/// `Lazy` implements `Clone` if `T::Params` does. Note that this only clones
/// the constructor parameters and not the object itself. These semantics are
/// appropriate for their use in allocator size classes, but maybe unintuitive
/// elsewhere. It implements `deref` and `deref_mut` to facilitate access to
/// the underlying object.
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
    /// Create a new `Lazy<T>` with stored constructor parameters given by `params`.
    pub fn new(params: T::Params) -> Self {
        Lazy {
            params,
            val: UnsafeCell::new(None),
        }
    }

    #[cfg_attr(feature = "cargo-clippy", allow(inline_always))]
    #[inline(always)]
    fn deref<P: UntypedObjectAlloc>(&self, handle: &Handle, pages: &mut P) -> Option<&T> {
        let state = unsafe { &mut *self.val.get() };
        if unsafe { unlikely(state.is_none()) } {
            *state = Some(T::init(&self.params, handle, pages)?);
        }
        Some(state.as_ref().unwrap())
    }

    #[cfg_attr(feature = "cargo-clippy", allow(inline_always))]
    #[inline(always)]
    fn deref_mut<P: UntypedObjectAlloc>(&mut self, handle: &Handle, pages: &mut P) -> Option<&mut T> {
        let state = unsafe { &mut *self.val.get() };
        if unsafe { unlikely(state.is_none()) } {
            *state = Some(T::init(&mut self.params, handle, pages)?);
        }
        Some(state.as_mut().unwrap())
    }
}

unsafe impl<T: LazyInitializable> AllocWith for Lazy<T>
where T: AllocWith
{
    unsafe fn alloc_with<P: UntypedObjectAlloc>(&mut self, handle: &Handle, pages: &mut P) -> Option<NonNull<u8>> {
        Lazy::deref_mut(self, handle, pages)?.alloc_with(handle, pages)
    }

    unsafe fn dealloc_with<P: UntypedObjectAlloc>(&mut self, handle: &Handle, pages: &mut P, ptr: NonNull<u8>) {
        // TODO: What to do if we get OOM while initializing?
        Lazy::deref_mut(self, handle, pages).unwrap().dealloc_with(handle, pages, ptr);
    }

    unsafe fn pre_drop<P: UntypedObjectAlloc>(&mut self, handle: &Handle, pages: &mut P) {
        let state = unsafe { &mut *self.val.get() };
        if unsafe { unlikely(state.is_none()) } {
            // hasn't been initialized, so there's no need to drop
            return;
        }
        state.as_mut().unwrap().pre_drop(handle, pages);
    }
}

impl<T: LazyInitializable> TryCloneWith for Lazy<T>
where
    T: TryCloneWith,
    T::Params: TryClone,
{
    fn try_clone_with<P: UntypedObjectAlloc>(&self, handle: &Handle, pages: &mut P) -> Option<Self> {
        let state = unsafe { &mut *self.val.get() };
        let params = self.params.try_clone()?;
        if unsafe { unlikely(state.is_none()) } {
            // hasn't been initialized, so we can just create a new instance
            // rather than cloning this one
            Some(Self::new(params))
        } else {
            let inner = state.as_ref().unwrap().try_clone_with(handle, pages)?;
            Some(Lazy { params, val: UnsafeCell::new(Some(inner)) })
        }
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

/// A simple mmap-based vector.
/// 
/// `MmapVec` is like `Vec`, but backed by pages allocated from mmap, and with
/// a much less complete API. The fact that pages are allocated from mmap means
/// that `MmapVec` is lazy: the pages are only backed by physical memory once
/// they are used.
/// 
/// Like `Vec`, `MmapVec` keeps an internal count of the current length of the
/// vector. Unlike `Vec`, however, it is valid to use the space beyond that
/// length so long as it does not overrun the capacity of the vector. This is
/// useful when the caller is particularly performance-sensitive and can
/// guarantee on their own that accesses are in bounds. Note, however, that
/// only elements `0` through `len - 1` will be dropped when the `MmapVec` is
/// dropped.
pub struct MmapVec<T> {
    raw: RawVec<T, MapAlloc>,
    len: usize,
}

impl<T> MmapVec<T> {
    pub fn new(cap: usize) -> Option<MmapVec<T>> {
        let layout = Layout::array::<T>(cap);
        let mut alloc = MapAlloc::default();
        let data = alloc.alloc(layout)?;
        Some(MmapVec {
            raw: RawVec::from_raw_parts(data, cap, alloc),
            len: 0,
        })
    }

    /// Push a new element onto the vector, increasing the length.
    pub unsafe fn push_debug_checked(&mut self, val: T) {
        alloc_debug_assert!(self.len < self.raw.cap());
        ptr::write(self.raw.ptr().offset(self.len), t);
        self.len += 1;
    }

    /// Get a reference to the index `idx`.
    /// 
    /// `get_debug_checked` gets a reference to the `idx`th element. `idx` must
    /// be within the capacity configured when this `MmapVec` was constructed,
    /// but `idx` is *not* required to be less than the current length of the
    /// vector.
    pub unsafe fn get_debug_checked(&mut self, idx: usize) -> *mut T {
        alloc_debug_assert!(idx < self.raw.cap());
        &mut *self.raw.ptr().offset(idx)
    }

    /// The number of elements that have been pushed onto this vector using
    /// `push_debug_checked`.
    pub fn len(&self) -> usize {
        self.len
    }

    /// The capacity that this vector was initially configured with.
    pub fn cap(&self) -> usize {
        self.raw.cap()
    }
}

impl<T: TryCloneWith> TryCloneWith for MmapVec<T> {
    fn try_clone_with<P: UntypedObjectAlloc>(&self, handle: &Handle, pages: &mut P) -> Option<Self> {
        let new = Self::new(self.len);
        for i in 0..self.len {
            // TODO: Cleanup previous entries if try_clone_with fails
            let new_t = self.get_debug_checked(i).try_clone_with(handle, pages)?;
            new.push_unchecked(new_t)
        }
        Some(new)
    }
}

impl<T> Drop for MmapVec<T> {
    /// Drop any elements pushed onto the vector using `push_debug_checked`.
    /// 
    /// Note that any elements written (e.g., using `get_debug_checked`) beyond
    /// the length of the vector are not dropped.
    fn drop(&mut self) {
        for i in 0..self.len {
            ptr::drop_in_pace(self.raw.ptr().offset(i))
        }
    }
}

impl<T> TypedArray<T> {
    pub fn new(size: usize) -> TypedArray<T> {
        use std::mem::size_of;
        let page_size = mmap::page_size();
        let bytes = size_of::<T>() * size;
        let rem = bytes % page_size;
        let n_pages = bytes / page_size + cmp::min(1, rem);
        let region_size = n_pages * page_size;
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

impl<T: TryCloneWith> TryCloneWith for TypedArray<T> {
    fn try_clone_with<P: UntypedObjectAlloc>(&self, handle: &Handle, pages: &mut P) -> Option<Self> {
        let new = Self::new(self.len);
        for i in 0..self.len {
            // TODO: Cleanup previous entries if try_clone_with fails
            let new_t = (*self.get(i)).try_clone_with(handle, pages)?;
            ptr::write(new.get(i), new_t);
        }
        Some(new)
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

/// Convert a `NonNull<T>` into an immutable reference with no lifetime bounds.
pub unsafe fn as_ref<'a, T>(ptr: NonNull<T>) -> &'a T {
    &*ptr.as_ptr()
}

/// Convert a `NonNull<T>` into a mutable reference with no lifetime bounds.
pub unsafe fn as_mut<'a, T>(ptr: NonNull<T>) -> &'a mut T {
    &mut *ptr.as_ptr()
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
        alloc_assert_eq!(l_u, 0);
        *l = DefaultInit(1);
        let l_u = l.0;
        alloc_assert_eq!(l_u, 1);
    }

}
