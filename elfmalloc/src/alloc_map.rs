// Copyright 2018 the authors. See the 'Copyright and license' section of the
// README.md file at the top-level directory of this repository.
//
// Licensed under the Apache License, Version 2.0 (the LICENSE-APACHE file) or
// the MIT license (the LICENSE-MIT file) at your option. This file may not be
// copied, modified, or distributed except according to those terms.

use std::cmp;
use std::marker::PhantomData;

use backing::GCAlloc;
use general::ELFMALLOC_SMALL_CUTOFF;
use util::{Const, LazyGuard, MmapVec, PreDrop, TryClone};

/// A trait encapsulating the notion of an array of size classes for an allocator.
pub trait AllocMap
where
    Self: Sized,
{
    /// The type stored in each size class.
    type T;
    /// The type used to index size classes.
    type Key;

    /// Create and initialize the map.
    fn init<F: FnMut(Self::Key) -> Self::T>(start: Self::Key, n_classes: usize, f: F) -> Option<Self>;

    /// Get an unchecked raw pointer to the class corresponding to `k`.
    unsafe fn get_raw(&mut self, k: Self::Key) -> *mut Self::T;

    /// Get an unchecked mutable reference to the class corresponding to `k`.
    #[cfg_attr(feature = "cargo-clippy", allow(inline_always))]
    #[inline(always)]
    unsafe fn get_mut(&mut self, k: Self::Key) -> &mut Self::T {
        &mut *self.get_raw(k)
    }

    /// Get the `Key` with a "maximum" value.
    ///
    /// This method is most useful when the `Key` type is a numeric type representing a "size
    /// class".
    fn max_key(&self) -> Self::Key;
}

// Note on the C API:
//
// The C allocation API guarantees a minimum alignment for all allocations. On some systems, this
// 8, while on others, 16. By default, our minimum size class is 8 bytes in size and all
// allocations are 8 byte aligned. On systems where the minimum alignment is 8, this means that we
// don't need to explicitly round up allocation size - the returned objects will always be properly
// aligned. However, on systems where the minimum alignment is 16, more work needs to be done.
// Thus, on these systems, when the "c-api" feature is enabled, we eliminate the 8-byte size class,
// making the smallest size class 16, and thus retaining this "aligned for free" property.

/// Size classes from the `scalloc` and `tcmalloc` allocators.
///
/// This includes two runs of size classes: the first (smaller) size classes are multiples of 16.
/// The larger classes are powers of two.
pub struct TieredSizeClasses<S, M, SB, MB, Mul> 
where
    S: PreDrop<SB>,
    M: PreDrop<MB>,
    SB: GCAlloc,
    MB: GCAlloc,
    Mul: Const<usize>,
{
    // When compiling for the C API, the minimum alignment is 16 on Mac and 64-bit Windows.
    #[cfg(any(not(feature = "c-api"),
                not(any(target_os = "macos", all(windows, target_pointer_width = "64")))))]
    word_objs: S,
    small_objs: Multiples<S, Mul>,
    medium_objs: PowersOfTwo<M>,
    small_backing: SB,
    medium_backing: MB,
    _marker: PhantomData<Mul>,
}

impl<S, M, SB, MB, Mul> TieredSizeClasses<S, M, SB, MB, Mul> 
where
    S: PreDrop<SB>,
    M: PreDrop<MB>,
    SB: GCAlloc,
    MB: GCAlloc,
    Mul: Const<usize>,
{
    pub fn new<FS, FM>(start: usize, small_backing: SB, medium_backing: MB, n_classes: usize, mut init_small: FS, mut init_medium: FM) -> Option<Self>
    where
        FS: FnMut(usize, usize) -> S,
        FM: FnMut(usize, usize) -> M,
    {
        let n_small_classes = cmp::min((ELFMALLOC_SMALL_CUTOFF / Mul::VAL) - (start / Mul::VAL), n_classes / 2);
        let n_medium_classes = n_classes - n_small_classes;

        let small_backing_size = small_backing.layout().size();
        let small_classes = Multiples::init(
                                start,
                                n_small_classes,
                                |obj_size| init_small(obj_size, small_backing_size),
                            )?;
        let medium_backing_size = medium_backing.layout().size();
        let medium_classes = PowersOfTwo::init(
                                small_classes.max_key() + 1,
                                n_medium_classes,
                                |obj_size| init_medium(obj_size, medium_backing_size),
                            )?;
        Some(TieredSizeClasses {
            // When compiling for the C API, the minimum alignment is 16 on Mac and 64-bit Windows.
            #[cfg(any(not(feature = "c-api"),
                        not(any(target_os = "macos",
                                    all(windows, target_pointer_width = "64")))))]
            word_objs: init_small(8, small_backing_size),
            small_objs: small_classes,
            medium_objs: medium_classes,
            small_backing,
            medium_backing,
            _marker: PhantomData,
        })
    }

    pub fn with<FS, FM, O>(&mut self, n: usize, small: FS, medium: FM) -> O
    where
        FS: Fn(&mut S, &LazyGuard, &mut SB) -> O,
        FM: Fn(&mut M, &LazyGuard, &mut MB) -> O,
    {
        let guard = LazyGuard::new();
        #[cfg(any(not(feature = "c-api"),
                    not(any(target_os = "macos",
                                all(windows, target_pointer_width = "64")))))]
        {
            if n <= 8 {
                return small(&mut self.word_objs, &guard, &mut self.small_backing);
            }
        }

        unsafe {
            if n <= self.small_objs.max_key() {
                small(self.small_objs.get_mut(n), &guard, &mut self.small_backing)
            } else {
                medium(self.medium_objs.get_mut(n), &guard, &mut self.medium_backing)
            }
        }
    }

    pub fn max_size(&self) -> usize {
        self.medium_objs.max_key()
    }

    pub fn small_backing(&self) -> &SB {
        &self.small_backing
    }

    pub fn medium_backing(&self) -> &MB {
        &self.medium_backing
    }
}

impl<S, M, SB, MB, Mul> TryClone for TieredSizeClasses<S, M, SB, MB, Mul>
where
    S: PreDrop<SB> + TryClone,
    M: PreDrop<MB> + TryClone,
    SB: GCAlloc + TryClone,
    MB: GCAlloc + TryClone,
    Mul: Const<usize>,
{
    fn try_clone(&self) -> Option<Self> {
        // TODO: Avoid leaking resources on failure (see issue #179)
        let small_backing = self.small_backing.try_clone()?;
        let medium_backing = self.medium_backing.try_clone()?;
        #[cfg(any(not(feature = "c-api"),
                        not(any(target_os = "macos", all(windows, target_pointer_width = "64")))))]
        let word_objs = self.word_objs.try_clone()?;
        let small_objs = self.small_objs.try_clone()?;
        let medium_objs = self.medium_objs.try_clone()?;

        Some(TieredSizeClasses {
            #[cfg(any(not(feature = "c-api"),
                        not(any(target_os = "macos", all(windows, target_pointer_width = "64")))))]
            word_objs,
            small_objs,
            medium_objs,
            small_backing,
            medium_backing,
            _marker: PhantomData,
        })
    }
}

impl<S, M, SB, MB, Mul> Drop for TieredSizeClasses<S, M, SB, MB, Mul>
where
    S: PreDrop<SB>,
    M: PreDrop<MB>,
    SB: GCAlloc,
    MB: GCAlloc,
    Mul: Const<usize>,
{
    fn drop(&mut self) {
        unsafe {
            let guard = LazyGuard::new();
            #[cfg(any(not(feature = "c-api"),
                not(any(target_os = "macos", all(windows, target_pointer_width = "64")))))]
            self.word_objs.pre_drop(&guard, &mut self.small_backing);
            self.small_objs.pre_drop(&guard, &mut self.small_backing);
            self.medium_objs.pre_drop(&guard, &mut self.medium_backing);
        }
    }
}

/// An array of size classes where sizes are multiples of 16.
pub struct Multiples<T, M: Const<usize>> {
    starting_size: usize,
    max_size: usize,
    classes: MmapVec<T>,
    _marker: PhantomData<M>,
}

/// Round up to the closest multiple of `M::VAL` greater than or equal to `n`.
#[inline]
fn round_up<M: Const<usize>>(n: usize) -> usize {
    (n + (M::VAL - 1)) & !(M::VAL - 1)
}

impl<T, M: Const<usize>> AllocMap for Multiples<T, M> {
    type T = T;
    type Key = usize;

    fn init<F: FnMut(usize) -> T>(start: usize, n_classes: usize, mut f: F) -> Option<Self> {
        alloc_debug_assert!(n_classes >= 1);
        let starting_size = round_up::<M>(start);
        let mut res = Multiples {
            starting_size: starting_size,
            max_size: n_classes * M::VAL + starting_size - M::VAL,
            classes: MmapVec::new(n_classes)?,
            _marker: PhantomData,
        };
        let mut cur_size = res.starting_size;
        for _ in 0..n_classes {
            res.classes.push(f(cur_size));
            cur_size += M::VAL;
        }
        alloc_debug_assert_eq!(res.max_size, cur_size - M::VAL);
        Some(res)
    }

    #[cfg_attr(feature = "cargo-clippy", allow(inline_always))]
    #[inline(always)]
    unsafe fn get_raw(&mut self, n: usize) -> *mut T {
        let class = round_up::<M>(n);
        alloc_debug_assert!(class <= self.max_size);
        self.classes.get_mut_debug_checked(
            (round_up::<M>(n) - self.starting_size) / M::VAL,
        )
    }

    #[inline]
    fn max_key(&self) -> usize {
        self.max_size
    }
}

impl<T: TryClone, M: Const<usize>> TryClone for Multiples<T, M> {
    fn try_clone(&self) -> Option<Self> {
        // TODO: Avoid leaking resources on failure (see issue #179)
        Some(Multiples {
            starting_size: self.starting_size,
            max_size: self.max_size,
            classes: self.classes.try_clone()?,
            _marker: PhantomData,
        })
    }
}

unsafe impl<T, B, M: Const<usize>> PreDrop<B> for Multiples<T, M>
where
    T: PreDrop<B>
{
    unsafe fn pre_drop(&mut self, guard: &LazyGuard, backing: &mut B) {
        self.classes.foreach(|class| class.pre_drop(guard, backing));
    }
}

/// Size classes that are just the powers of two.
///
/// This is useful mostly for testing purposes: it is a very simple implementation, but it can also
/// be rather wasteful.
pub struct PowersOfTwo<T> {
    starting_size: usize,
    max_size: usize,
    classes: MmapVec<T>,
}

impl<T> PowersOfTwo<T> {
    fn new(start_from: usize, n_classes: usize) -> Option<PowersOfTwo<T>> {
        Some(PowersOfTwo {
            starting_size: start_from.next_power_of_two(),
            max_size: 0, // currently uninitialized
            classes: MmapVec::new(n_classes)?,
        })
    }
}

impl<T> AllocMap for PowersOfTwo<T> {
    type T = T;
    type Key = usize;

    fn init<F: FnMut(Self::Key) -> T>(
        start: usize,
        n_classes: usize,
        mut f: F,
    ) -> Option<Self> {
        let mut res = Self::new(start, n_classes)?;
        let mut cur_size = res.starting_size;
        for _ in 0..n_classes {
            res.classes.push(f(cur_size));
            cur_size *= 2;
        }
        res.max_size = cur_size / 2;
        Some(res)
    }

    #[cfg_attr(feature = "cargo-clippy", allow(inline_always))]
    #[inline(always)]
    unsafe fn get_raw(&mut self, k: usize) -> *mut T {
        alloc_debug_assert!(k <= self.max_size);
        let log = (k.next_power_of_two().trailing_zeros() -
            self.starting_size.trailing_zeros()) as usize;
        self.classes.get_mut_debug_checked(log)
    }

    #[inline]
    fn max_key(&self) -> usize {
        self.max_size
    }
}

impl<T: TryClone> TryClone for PowersOfTwo<T> {
    fn try_clone(&self) -> Option<Self> {
        // TODO: Avoid leaking resources on failure (see issue #179)
        Some(PowersOfTwo {
            starting_size: self.starting_size,
            max_size: self.max_size,
            classes: self.classes.try_clone()?,
        })
    }
}

unsafe impl<T, B> PreDrop<B> for PowersOfTwo<T>
where
    T: PreDrop<B>
{
    unsafe fn pre_drop(&mut self, guard: &LazyGuard, backing: &mut B) {
        self.classes.foreach(|class| class.pre_drop(guard, backing));
    }
}