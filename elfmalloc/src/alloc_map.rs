use std::cmp;
use std::marker::PhantomData;
use std::ptr;

use object_alloc::UntypedObjectAlloc;

use frontends::Frontend;
use general::{ELFMALLOC_SMALL_CUTOFF, ELFMALLOC_SMALL_PAGE_SIZE};
use util::{Const, MmapVec, TryClone, TryCloneWith};

/// A trait encapsulating the notion of an array of size classes for an allocator.
pub trait AllocMap
where
    Self: Sized,
{
    /// The type stored in each size class.
    type T;
    /// The type used to index size classes.
    type Key;

    /// Create and initialize the map, returning ownership of the constructor.
    fn init<F: FnMut(Self::Key) -> Self::T>(start: Self::Key, n_classes: usize, f: F) -> Option<(F, Self)>;

    /// Get an unchecked raw pointer to the class corresponding to `k`.
    unsafe fn get_raw(&self, k: Self::Key) -> *mut Self::T;

    /// Get an unchecked reference to the class corresponding to `k`.
    #[cfg_attr(feature = "cargo-clippy", allow(inline_always))]
    #[inline(always)]
    unsafe fn get(&self, k: Self::Key) -> &Self::T {
        &*self.get_raw(k)
    }

    /// Get an unchecked mutable reference to the class corresponding to `k`.
    #[cfg_attr(feature = "cargo-clippy", allow(inline_always))]
    #[inline(always)]
    unsafe fn get_mut(&mut self, k: Self::Key) -> &mut Self::T {
        &mut *self.get_raw(k)
    }

    /// Iterate over the map's contents.
    ///
    /// This is used to clean up the contents of the map.
    // fn foreach<F: Fn(*mut Self::T)>(&self, f: F);

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
pub struct TieredSizeClasses<FE, SP, MP, M> 
where
    SP: UntypedObjectAlloc,
    MP: UntypedObjectAlloc,
    M: Const<usize>,
{
    // When compiling for the C API, the minimum alignment is 16 on Mac and 64-bit Windows.
    #[cfg(any(not(feature = "c-api"),
                not(any(target_os = "macos", all(windows, target_pointer_width = "64")))))]
    word_objs: Option<FE>,
    small_objs: Multiples<FE, M>,
    medium_objs: PowersOfTwo<FE>,
    small_pages: SP,
    medium_pages: MP,
    _marker: PhantomData<M>,
}

impl<FE, SP, MP, M> TieredSizeClasses<FE, SP, MP, M>
where
    SP: UntypedObjectAlloc,
    MP: UntypedObjectAlloc,
    M: Const<usize>,
{
    fn new<F>(start: usize, small_pages: SP, medium_pages: MP, n_classes: usize, f: F) -> Option<TieredSizeClasses<FE, SP, MP, M>>
    where
        F: FnMut(usize) -> FE
    {
        let n_small_classes = cmp::min((ELFMALLOC_SMALL_CUTOFF / M::VAL) - (start / M::VAL), n_classes / 2);
        let n_medium_classes = n_classes - n_small_classes;
        let (f, small_classes) = Multiples::init(start, n_small_classes, f)?;
        // mutability is unnecessary when we don't execute the 'let word_objs = f3(8)' line
        #[allow(unused_mut)]
        let (mut f, medium_classes) =
            PowersOfTwo::init(small_classes.max_key() + 1, n_medium_classes, f)?;
        #[cfg(any(not(feature = "c-api"),
                    not(any(target_os = "macos",
                                all(windows, target_pointer_width = "64")))))]
        let word_objs = f(8);
        Some(TieredSizeClasses {
            // When compiling for the C API, the minimum alignment is 16 on Mac and 64-bit Windows.
            #[cfg(any(not(feature = "c-api"),
                        not(any(target_os = "macos",
                                    all(windows, target_pointer_width = "64")))))]
            word_objs: Some(word_objs),
            small_objs: small_classes,
            medium_objs: medium_classes,
            small_pages,
            medium_pages,
            _marker: PhantomData,
        })
    }

    pub fn with<SF, MF, O>(&mut self, n: usize, small: SF, medium: MF) -> O
    where
        SF: Fn(&mut FE, &mut SP) -> O,
        MF: Fn(&mut FE, &mut MP) -> O,
    {
        #[cfg(any(not(feature = "c-api"),
                    not(any(target_os = "macos",
                                all(windows, target_pointer_width = "64")))))]
        {
            if n <= 8 {
                return small(self.word_objs.as_mut().unwrap(), &mut self.small_pages);
            }
        }

        if n <= self.small_objs.max_key() {
            small(&mut *self.small_objs.get_raw(n), &mut self.small_pages)
        } else {
            medium(&mut *self.medium_objs.get_raw(n), &mut self.medium_pages)
        }
    }

    // unsafe fn get_raw(&self, n: usize) -> *mut T {
    //     // When compiling for the C API, the minimum alignment is 16 on Mac and 64-bit Windows.
    //     #[cfg(any(not(feature = "c-api"),
    //                 not(any(target_os = "macos",
    //                             all(windows, target_pointer_width = "64")))))]
    //     {
    //         if n <= 8 {
    //             self.word_objs.as_ref().unwrap() as *const _ as *mut T
    //         } else if n <= self.small_objs.max_key() {
    //             self.small_objs.get_raw(n)
    //         } else {
    //             self.medium_objs.get_raw(n)
    //         }
    //     }

    //     #[cfg(all(feature = "c-api",
    //                 any(target_os = "macos", all(windows, target_pointer_width = "64"))))]
    //     {
    //         if n <= self.small_objs.max_key() {
    //             self.small_objs.get_raw(n)
    //         } else {
    //             self.medium_objs.get_raw(n)
    //         }
    //     }
    // }

    pub fn max_size(&self) -> usize {
        self.medium_objs.max_key()
    }

    // fn foreach<F: Fn(*mut FE)>(&self, f: F) {
    //     #[cfg(any(not(feature = "c-api"),
    //                 not(any(target_os = "macos",
    //                             all(windows, target_pointer_width = "64")))))]
    //     {
    //         if let Some(r) = self.word_objs.as_ref() {
    //             f(r as *const _ as *mut FE);
    //         }
    //     }
    //     self.small_objs.foreach(&f);
    //     self.medium_objs.foreach(f);
    // }
}

impl<FE, SP, MP, M> TryClone for TieredSizeClasses<FE, SP, MP, M>
where
    FE: TryCloneWith,
    SP: UntypedObjectAlloc,
    MP: UntypedObjectAlloc,
    M: Const<usize>,
{
    fn try_clone(&self) -> Option<Some> {
        let mut small_pages = self.small_pages.try_clone()?;
        let mut medium_pages = self.medium_pages.try_clone()?;
        let handle = self.handle.clone();
        #[cfg(any(not(feature = "c-api"),
                        not(any(target_os = "macos", all(windows, target_pointer_width = "64")))))]
        let word_objs = self.word_objs.try_clone_with(&handle, &mut small_pages);
        let small_objs = self.small_objs.try_clone_with(&handle, &mut small_pages);
        let medium_objs = self.medium_objs.try_clone_with(&handle, &mut medium_pages);

        Some(TieredSizeClasses {
            #[cfg(any(not(feature = "c-api"),
                        not(any(target_os = "macos", all(windows, target_pointer_width = "64")))))]
            word_objs,
            small_objs,
            medium_objs,
            small_pages,
            medium_pages,
            _marker: PhantomData,
        })
    }
}

/// An array of size classes where sizes are multiples of 16.
struct Multiples<T, M: Const<usize>> {
    starting_size: usize,
    max_size: usize,
    classes: MmapVec<T>,
    _marker: PhantomData<M>,
}

impl<T: Clone, M: Const<usize>> Clone for Multiples<T, M> {
    fn clone(&self) -> Self {
        Multiples::init(self.starting_size, self.classes.len(), |size| unsafe {
            self.get(size).clone()
        })
    }
}

/// Round up to the closest multiple of `M::VAL` greater than or equal to `n`.
#[inline]
fn round_up<M: Const<usize>>(n: usize) -> usize {
    (n + (M::VAL - 1)) & !(M::VAL - 1)
}

impl<T, M: Const<usize>> AllocMap for Multiples<T, M> {
    type T = T;
    type Key = usize;

    fn init<F: FnMut(usize) -> T>(start: usize, n_classes: usize, mut f: F) -> Option<(F, Self)> {
        alloc_debug_assert!(n_classes >= 1);
        let starting_size = round_up(start);
        let res = Multiples {
            starting_size: starting_size,
            max_size: n_classes * M::VAL + starting_size - M::VAL,
            classes: MmapVec::new(n_classes)?,
            _marker: PhantomData,
        };
        let mut cur_size = res.starting_size;
        for _ in 0..n_classes {
            unsafe { res.push_debug_checked(f(cur_size)) };
            cur_size += M::VAL;
        }
        // for p in res.classes.iter() {
        //     unsafe {
        //         ptr::write(p, f(cur_size));
        //     }
            
        // }
        alloc_debug_assert_eq!(res.max_size, cur_size - M::VAL);
        Some((f, res))
    }

    #[cfg_attr(feature = "cargo-clippy", allow(inline_always))]
    #[inline(always)]
    unsafe fn get_raw(&self, n: usize) -> *mut T {
        let class = round_up(n);
        alloc_debug_assert!(class <= self.max_size);
        self.classes.get_debug_checked(
            (round_up(n) - self.starting_size) / M::VAL,
        )
    }

    #[inline]
    fn max_key(&self) -> usize {
        self.max_size
    }

    // fn foreach<F: Fn(*mut T)>(&self, f: F) {
    //     for class in self.classes.iter() {
    //         f(class)
    //     }
    // }
}

impl<T: TryCloneWith, M: Const<usize>> TryCloneWith for Multiples<T, M> {
    fn try_clone_with<P: UntypedObjectAlloc>(&self, handle: &Handle, pages: &mut P) -> Option<Self> {
        Some(Multiples {
            starting_size: self.starting_size,
            max_size: self.max_size,
            classes: self.classes.try_clone_with(handle, pages)?,
            _marker: PhantomData,
        })
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
    ) -> Option<(F, Self)> {
        let mut res = Self::new(start, n_classes)?;
        let mut cur_size = res.starting_size;
        for _ in 0..n_classes {
            unsafe { res.classes.push_debug_checked(f(cur_size)) };
            cur_size *= 2;
        }
        // unsafe {
        //     for item in res.classes.iter() {
        //         ptr::write(item, f(cur_size));
        //         cur_size *= 2;
        //     }
        // }
        res.max_size = cur_size / 2;
        Some((f, res))
    }

    #[cfg_attr(feature = "cargo-clippy", allow(inline_always))]
    #[inline(always)]
    unsafe fn get_raw(&self, k: usize) -> *mut T {
        alloc_debug_assert!(k <= self.max_size);
        let log = (k.next_power_of_two().trailing_zeros() -
            self.starting_size.trailing_zeros()) as usize;
        self.classes.get_debug_checked(log)
    }

    #[inline]
    fn max_key(&self) -> usize {
        self.max_size
    }

    // fn foreach<F: Fn(*mut T)>(&self, f: F) {
    //     for class in self.classes.iter() {
    //         f(class)
    //     }
    // }
}

impl<T: TryCloneWith> TryCloneWith for PowersOfTwo<T> {
    fn try_clone_with<P: UntypedObjectAlloc>(&self, handle: &Handle, pages: &mut P) -> Option<Self> {
        Some(PowersOfTwo {
            starting_size: self.starting_size,
            max_size: self.max_size,
            classes: self.classes.try_clone_with(handle, pages)?,
        })
    }
}