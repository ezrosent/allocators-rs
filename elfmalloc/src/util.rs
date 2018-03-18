// Copyright 2018 the authors. See the 'Copyright and license' section of the
// README.md file at the top-level directory of this repository.
//
// Licensed under the Apache License, Version 2.0 (the LICENSE-APACHE file) or
// the MIT license (the LICENSE-MIT file) at your option. This file may not be
// copied, modified, or distributed except according to those terms.

//! Some basic utilities used throughout the allocator code.
use std::intrinsics::unlikely;
use std::mem::ManuallyDrop;
use std::ops::{Deref, DerefMut};
use std::ptr::{self, NonNull};

use alloc::allocator::{Alloc, Layout};
use alloc::raw_vec::RawVec;
use alloc_fmt::AllocUnwrap;
use mmap_alloc::MapAlloc;

pub mod mmap {
    use alloc::allocator::{Alloc, Layout};
    use alloc_fmt::AllocUnwrap;
    use mmap_alloc::{MapAlloc, MapAllocBuilder};

    lazy_static!{ 
        static ref MMAP: MapAlloc = MapAllocBuilder::default()
            .commit(cfg!(windows))
            .build();
    }

    pub fn page_size() -> usize {
        ::sysconf::page::pagesize()
    }

    pub fn map(size: usize) -> *mut u8 {
        fallible_map(size).alloc_expect("out of memory")
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
        Layout::from_size_align(size, page_size()).alloc_unwrap()
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
pub unsafe trait AllocWith<B> {
    unsafe fn alloc_with(&mut self, guard: &LazyGuard, backing: &mut B) -> Option<NonNull<u8>>;
    unsafe fn dealloc_with(&mut self, guard: &LazyGuard, backing: &mut B, ptr: NonNull<u8>);
}

pub unsafe trait PreDrop<B> {
    unsafe fn pre_drop(&mut self, guard: &LazyGuard, backing: &mut B);
}

pub trait TryCloneWith<B>
where Self: Sized
{
    fn try_clone_with(&self, guard: &LazyGuard, backing: &mut B) -> Option<Self>;
}

pub trait Const<T> {
    const VAL: T;
}

/// Types which can be built from a configuration.
/// 
/// Types which implement `ConfigBuild` can build themselves from a `Config`
/// configuration.
pub trait ConfigBuild<B>
where
    Self: Sized
{
    type Config;
    fn build(config: &Self::Config, guard: &LazyGuard, backing: &mut B) -> Option<Self>;
}

/// A lazily-initialized value.
/// 
/// A `Lazy` is a lazily-initialized `T`. It stores a configuration
/// (`T::Config`) and builds the `T` itself on demand.
/// 
/// `Lazy` implements `TryClone` if `T::Config` does. Note that cloning a `Lazy`
/// only clones the configuration and not the constructed `T` itself. These
/// semantics are appropriate for use in allocator size classes, but may be
/// unintuitive elsewhere. `Lazy` provides `deref_mut_with`, which is like the
/// similarly-named method from the `DerefMut` trait, but which take extra
/// parameters.
/// 
/// By a similar token, `Lazy` implements `Sync` if `T::Config` does. Since
/// `try_clone` is the only method that is immutable, and it only accesses the
/// configuration (not the `T` itself), this is safe.
pub struct Lazy<T, B>
where
    T: ConfigBuild<B>,
{
    config: T::Config,
    val: Option<T>,
}

impl<T, B> Lazy<T, B>
where
    T: ConfigBuild<B>,
{
    /// Create a new `Lazy<T>` with stored configuration `config`.
    pub fn new(config: T::Config) -> Self {
        Lazy {
            config,
            val: None,
        }
    }

    #[cfg_attr(feature = "cargo-clippy", allow(inline_always))]
    #[inline(always)]
    fn deref_mut_with(&mut self, guard: &LazyGuard, backing: &mut B) -> Option<&mut T> {
        if unsafe { unlikely(self.val.is_none()) } {
            self.val = Some(T::build(&mut self.config, guard, backing)?);
        }
        Some(self.val.as_mut().alloc_unwrap())
    }
}

unsafe impl<T, B> AllocWith<B> for Lazy<T, B>
where
    T: ConfigBuild<B> + AllocWith<B>,
{
    unsafe fn alloc_with(&mut self, guard: &LazyGuard, backing: &mut B) -> Option<NonNull<u8>> {
        self.deref_mut_with(guard, backing)?.alloc_with(guard, backing)
    }

    unsafe fn dealloc_with(&mut self, guard: &LazyGuard, backing: &mut B, ptr: NonNull<u8>) {
        // TODO: What to do if we get OOM while initializing? We should have a
        // better answer than just to leak ptr.
        self.deref_mut_with(guard, backing).alloc_unwrap().dealloc_with(guard, backing, ptr);
    }
}

unsafe impl<T, B> PreDrop<B> for Lazy<T, B>
where
    T: ConfigBuild<B> + PreDrop<B>,
{
    unsafe fn pre_drop(&mut self, guard: &LazyGuard, backing: &mut B) {
        if unlikely(self.val.is_none()) {
            // hasn't been initialized, so there's no need to drop
            return;
        }
        self.val.as_mut().alloc_unwrap().pre_drop(guard, backing);
    }
}

impl<T, B> TryClone for Lazy<T, B>
where
    T: ConfigBuild<B>,
    T::Config: TryClone,
{
    fn try_clone(&self) -> Option<Self> {
        // NOTE: Don't touch self.val since it's not guaranteed to be Sync!
        // TODO: Avoid leaking resources on failure (see issue #179)
        Some(Self::new(self.config.try_clone()?))
    }
}

unsafe impl<T, B> Sync for Lazy<T, B>
where
    T: ConfigBuild<B>,
    T::Config: Sync
{}

/// A simple mmap-based vector.
/// 
/// `MmapVec` is like `Vec`, but backed by pages allocated from mmap, and with a
/// much less complete API. The fact that pages are allocated from mmap means
/// that `MmapVec` is lazy: the pages are only backed by physical memory once
/// they are used.
/// 
/// Like `Vec`, `MmapVec` keeps an internal count of the current length of the
/// vector. Unlike `Vec`, however, it is valid to use the space beyond that
/// length so long as it does not overrun the capacity of the vector. This is
/// useful when the caller is particularly performance-sensitive and can
/// guarantee on their own that accesses are in bounds. Note, however, that only
/// elements `0` through `len - 1` will be dropped when the `MmapVec` is
/// dropped.
pub struct MmapVec<T> {
    raw: RawVec<T, MapAlloc>,
    len: usize,
    #[cfg(debug_assertions)]
    dropped: bool,
}

impl<T> MmapVec<T> {
    pub fn new(cap: usize) -> Option<MmapVec<T>> {
        unsafe {
            let layout = Layout::array::<T>(cap).alloc_unwrap();
            let mut alloc = MapAlloc::default();
            // RawVec doesn't suppor fallible initialization, so explicitly
            // allocate first
            let data = Alloc::alloc(&mut alloc, layout).ok()?;
            Some(MmapVec {
                raw: RawVec::from_raw_parts_in(data as *mut _, cap, alloc),
                len: 0,
                #[cfg(debug_assertions)]
                dropped: false,
            })
        }
    }

    /// Push a new element onto the vector, increasing the length.
    /// 
    /// # Panics
    /// 
    /// `push` `alloc_assert!`s that the operation will not result in the
    /// vector's length exceeding its capacity.
    pub fn push(&mut self, val: T) {
        alloc_assert!(self.len < self.raw.cap());
        unsafe { ptr::write(self.raw.ptr().offset(self.len as isize), val) };
        self.len += 1;
    }

    /// Push a new element onto the vector, increasing the length.
    /// 
    /// # Safety
    /// 
    /// If the vector's length is already equal to its capacity, the behavior of
    /// `push_debug_checked` is undefined.
    pub unsafe fn push_debug_checked(&mut self, val: T) {
        alloc_debug_assert!(self.len < self.raw.cap());
        ptr::write(self.raw.ptr().offset(self.len as isize), val);
        self.len += 1;
    }

    /// Get a mutable reference to the index `idx`.
    /// 
    /// `get_debug_checked` gets a reference to the `idx`th element. `idx` must
    /// be within the capacity configured when this `MmapVec` was constructed,
    /// but `idx` is *not* required to be less than the current length of the
    /// vector.
    /// 
    /// # Safety
    /// 
    /// If `idx` is not within bounds of the vector's length, the returned
    /// pointer is not guaranteed to point to a valid instance of `T`. However,
    /// so long as `idx` is within bounds of the vector's capacity, it *is*
    /// guaranteed to point to allocated memory of size sufficient to hold a
    /// `T`. If `idx` is not within bounds of the vector's capacity, the
    /// behavior of `get_mut_debug_checked` is undefined.
    pub unsafe fn get_mut_debug_checked(&mut self, idx: usize) -> *mut T {
        alloc_debug_assert!(idx < self.raw.cap());
        self.raw.ptr().offset(idx as isize)
    }

    /// Get an immutable reference to the index `idx`.
    /// 
    /// `get_debug_checked` gets a reference to the `idx`th element. `idx` must
    /// be within the capacity configured when this `MmapVec` was constructed,
    /// but `idx` is *not* required to be less than the current length of the
    /// vector.
    /// 
    /// # Safety
    /// 
    /// If `idx` is not within bounds of the vector's length, the returned
    /// pointer is not guaranteed to point to a valid instance of `T`. However,
    /// so long as `idx` is within bounds of the vector's capacity, it *is*
    /// guaranteed to point to allocated memory of size sufficient to hold a
    /// `T`. If `idx` is not within bounds of the vector's capacity, the
    /// behavior of `get_mut_debug_checked` is undefined.
    pub unsafe fn get_debug_checked(&self, idx: usize) -> *const T {
        alloc_debug_assert!(idx < self.raw.cap());
        self.raw.ptr().offset(idx as isize)
    }

    /// The number of elements that have been pushed onto this vector using
    /// `push` or `push_debug_checked`.
    pub fn len(&self) -> usize {
        self.len
    }

    /// The capacity that this vector was initially configured with.
    pub fn cap(&self) -> usize {
        self.raw.cap()
    }

    /// Call a function for each element of the vector.
    /// 
    /// `foreach` invokes `f` once for each element of the vector that was
    /// pushed using `push` or `push_debug_checked`.
    pub fn foreach<F: FnMut(&mut T)>(&mut self, mut f: F) {
        unsafe {
            for i in 0..self.len {
                f(&mut *self.raw.ptr().offset(i as isize));
            }
        }
    }
}

impl<T: TryClone> TryClone for MmapVec<T> {
    /// Clone the elements pushed onto the vector using `push_debug_checked`.
    /// 
    /// If the clone succeeds, the new vector will have the same capacity as
    /// the old one, and all of the elements `0` through `len - 1` will be
    /// cloned into the new vector. All of the other memory locations in the
    /// new vector will be uninitialized.
    fn try_clone(&self) -> Option<Self> {
        let mut new = Self::new(self.len)?;
        for i in 0..self.len {
            // TODO: Cleanup previous entries if try_clone_with fails
            unsafe {
                let new_t = (*self.raw.ptr().offset(i as isize)).try_clone()?;
                new.push_debug_checked(new_t)
            }
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
        #[cfg(debug_assertions)]
        {
            alloc_debug_assert!(!self.dropped);
            self.dropped = true;
        }
        for i in 0..self.len {
            unsafe { ptr::drop_in_place(self.raw.ptr().offset(i as isize)) };
        }
    }
}

/// A wrapper which ensures that its contents are only dropped once.
/// 
/// Due to yet-undebugged issues with thread-local storage, values stored in
/// thread-local storage are sometimes dropped twice. `DropChecked` wraps a type
/// and keeps track of whether it has been dropped, ensuring that it is only
/// dropped once. If `debug_assertions` are enabled, it also emits a warning on
/// stderr when a value is dropped more than once.
pub struct DropChecked<T> {
    val: ManuallyDrop<T>,
    dropped: bool,
}

impl<T> DropChecked<T> {
    pub fn new(val: T) -> DropChecked<T> {
        DropChecked { val: ManuallyDrop::new(val), dropped: false }
    }
}

impl<T> Deref for DropChecked<T> {
    type Target = T;

    fn deref(&self) -> &T {
        self.val.deref()
    }
}

impl<T> DerefMut for DropChecked<T> {
    fn deref_mut(&mut self) -> &mut T {
        self.val.deref_mut()
    }
}

impl<T> Drop for DropChecked<T> {
    fn drop(&mut self) {
        unsafe {
            if self.dropped {
                if cfg!(debug_assertions) {
                    let name = ::std::intrinsics::type_name::<DropChecked<T>>();
                    alloc_eprintln!("value of type {} dropped multiple times", name);
                }
                return;
            }
            self.dropped = true;
            ManuallyDrop::drop(&mut self.val);
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

pub use self::gc::LazyGuard;

mod gc {
    use std::cell::UnsafeCell;

    use alloc_fmt::AllocUnwrap;
    use crossbeam_epoch::{Collector, Guard, Handle};

    // Currently, there's no way to get an epoch GC handle that is both Send and
    // Sync other than to only keep a Collector and re-register a Handle every
    // time one is needed (which is very expensive). This is our temporary
    // workaround.
    //
    // Similarly to the crossbeam_epoch crate itself, we store a default global
    // collector and handles to that collector in thread-local storage. Unlike
    // crossbeam_epoch, however, we explicitly handle:
    // - If the thread-local storage has already been destructed, we fall back
    //   on a slow path of registering a new handle.
    // - By using thread-local storage from the alloc-tls crate, we ensure that
    //   our thread-local storage is safe for use in a global allocator.
    //
    // Once Handles are Send and Sync, we can re-evaluate this appoach. See this
    // GitHub issue: https://github.com/crossbeam-rs/crossbeam-epoch/issues/73
    lazy_static!{ static ref COLLECTOR: Collector = Collector::new(); }
    alloc_thread_local!{ static HANDLE: Handle = COLLECTOR.register(); }

    fn pin() -> Guard {
        unsafe { HANDLE.with(Handle::pin).unwrap_or_else(|| COLLECTOR.register().pin()) }
    }

    /// A lazily-pinned `Guard`.
    /// 
    /// A `LazyGuard` is a lazily-pinned guard that is only dropped once the
    /// `LazyGuard` is dropped. This allows code to avoid pinning and then
    /// unpinning repeatedly and also avoid pinning unnecessarily.
    pub struct LazyGuard {
        val: UnsafeCell<Option<Guard>>,
    }

    impl LazyGuard {
        pub fn new() -> LazyGuard {
            LazyGuard { val: UnsafeCell::new(None) }
        }

        pub fn guard(&self) -> &Guard {
            let state = unsafe { &mut *self.val.get() };
            if state.is_none() {
                *state = Some(pin());
            }
            state.as_ref().alloc_unwrap()
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_mmap_vec() {
        let mut vec = MmapVec::new(16).unwrap();
        for i in 0..16 {
            unsafe { vec.push_debug_checked(i) };
        }
        for i in 0..16 {
            unsafe { alloc_assert_eq!(i, *vec.get_debug_checked(i)) };
        }
    }
}

// #[cfg(test)]
// mod tests {
//     use super::*;

//     #[derive(Debug)]
//     struct DefaultInit<T: Default>(T);
//     impl<T: Default> ConfigBuild for DefaultInit<T> {
//         type Config = ();
//         fn build<P: GCAlloc>(_cfg: &(), handle: &Handle, pages: &mut P) -> Option<Self> {
//             Some(DefaultInit(T::default()))
//         }
//     }

//     #[test]
//     fn basic_functionality() {
//         let mut l = Lazy::<DefaultInit<usize>>::new(());
//         let l_u = l.0;
//         alloc_assert_eq!(l_u, 0);
//         *l = DefaultInit(1);
//         let l_u = l.0;
//         alloc_assert_eq!(l_u, 1);
//     }
// }
