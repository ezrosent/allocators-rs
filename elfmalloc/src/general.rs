// Copyright 2017 the authors. See the 'Copyright and license' section of the
// README.md file at the top-level directory of this repository.
//
// Licensed under the Apache License, Version 2.0 (the LICENSE-APACHE file) or
// the MIT license (the LICENSE-MIT file) at your option. This file may not be
// copied, modified, or distributed except according to those terms.

//! Implementation of traditional `malloc`-style allocator routines based off of the `Slag`
//! allocator design.
//!
//! The primary use of this module is to provide the rudaments of a `malloc`-compatible global
//! allocator that can be used from C/C++ and Rust programs alike. The `elfc` crate that wraps
//! this one exposes such an interface. It is currently possible to use this module as a Rust
//! library, though we do not recommend it.
//!
//! # Using this Allocator from Rust
//!
//! We currently rely on some global allocator (bsalloc) to be running to service normal heap
//! allocations. As a result, this allocator cannot be used as a global allocator via the
//! `#[global_allocator]` attribute. Currently the only way around this is to use the `System`
//! allocator along with `libelfc` from the `elfc` crate loaded with `LD_PRELOAD`.
//!
//! It is also possible to use this allocator using a `Clone`-based API. As alluded to elsewhere,
//! the allocator is thread-safe and any handle on the allocator can be used to free a pointer from
//! any other handle in any other thread. If you `free` a pointer `alloc`-ed by another
//! `DynamicAllocator`, bad things will happen.
//!
//! ```rust,ignore
//! // all calls to `alloc` and `free` are unsafe
//! let mut elf = DynamicAllocator::new();
//! let ptr = elf.alloc(16) as *mut [u8; 16];
//! let mut elf_clone = elf.clone();
//! let res = thread::spawn(move || {
//!     elf_clone.alloc(24) as *mut [u8; 24]
//! }).join().unwrap();
//! elf.free(res);
//! elf.free(ptr);
//! ```
//!
//! This is probably a more limited use-case until custom allocators have better support in the
//! Rust ecosystem. Even then, we suspect most programmers using a non-global allocator will
//! instead want something more specialized, such as the `LocalAllocator` and `MagazineAllocator`
//! object-specific allocators.

use std::ptr;
use std::mem;

// One of MagazineCache and LocalCache is unused, depending on whether the 'local_cache' feature is
// enabled.
use super::sources::{MemorySource, MmapSource};
#[allow(unused_imports)]
use super::slag::{compute_metadata, CoarseAllocator, DirtyFn, LocalCache, MagazineCache, Metadata,
                  PageAlloc, RevocablePipe, Slag, PageCleanup};
use super::utils::{mmap, Lazy, TypedArray, likely};
use super::alloc_type::AllocType;

type Source = MmapSource;

pub mod global {
    //! A global malloc-style interface to interact with a `DynamicAllocator`. All of these
    //! structures are lazily initailized.
    //!
    //! One could be forgiven for thinking that this could work by simply using a global
    //! `lazy_static`-managed instance of a `DynamicAllocator` and then using thread-local storage
    //! (TLS) to store handles to this global instance. While this is essentially the architecture
    //! we use, a number of hacks have been added to ensure correctness.
    //!
    //! ## TLS Destructors
    //!
    //! Thread-local handles are stored in TLS. In their destructors, they potentially call into
    //! crossbeam code. This code too requires the use of TLS. We are not guaranteed any order in
    //! which these destructors can be run, and we have observed that crossbeam's can be run before
    //! ours, resulting in a panic.
    //!
    //! To avoid this we spawn a background thread that services `free` operations sent from
    //! threads in circumstances like this. While this is undoubtedly a code smell, it may be used
    //! in the future to collect statistics regarding the running allocator.
    //!
    //! ## Recursive `malloc` calls
    //!
    //! When used as a standard `malloc` implementation through the `elfc` crate via `LD_PRELOAD`,
    //! all calls to `malloc` and related functions will be routed through this module. The only
    //! problem is that the code that enqueues destructors for pthread TSD calls `calloc`; this
    //! causes all such calls to stack overflow.
    //!
    //! The fix for this is to use the thread-local attribute to create a thread-local boolean that
    //! indicates if the current thread's value has been initialized. If this value is false, a
    //! slower fallback algorithm is used.
    #[allow(unused_imports)]
    use super::{CoarseAllocator, DynamicAllocator, DirtyFn, ElfMalloc, MemorySource, ObjectAlloc,
                PageAlloc, TieredSizeClasses, TypedArray, AllocType, get_type, Source, AllocMap};
    #[cfg(feature = "nightly")]
    use super::likely;
    use std::ptr;
    use std::cell::UnsafeCell;
    use std::mem;
    #[allow(unused_imports)]
    use std::sync::atomic::{AtomicUsize, Ordering};
    use std::sync::mpsc::{channel, Sender};
    use std::sync::Mutex;
    use std::thread;

    type PA = PageAlloc<Source, ()>;
    // For debugging purposes: run a callback to eagerly dirty several pages. This is generally bad
    // for performance.
    //
    // type PA = PageAlloc<Source, BackgroundDirty>;

    unsafe fn dirty_slag(mem: *mut u8) {
        trace!("dirtying {:?}", mem);
        let usable_size = 32 << 10;
        let base_page = 4096;
        let mut cur_addr = mem.offset(base_page);
        while cur_addr < mem.offset(usable_size) {
            cur_addr = cur_addr.offset(base_page);
            (*(cur_addr as *mut AtomicUsize)).compare_and_swap(0, 1, Ordering::Relaxed);
        }
    }

    #[derive(Clone)]
    struct BackgroundDirty;
    impl DirtyFn for BackgroundDirty {
        fn dirty(_mem: *mut u8) {
            #[cfg(feature = "nightly")]
            {
                let _ = LOCAL_DESTRUCTOR_CHAN.try_with(|h| h.send(Husk::Slag(_mem)));
            }
        }
    }

    #[cfg(all(feature = "nightly", target_thread_local))]
    #[thread_local]
    /// A thread-local value used to guard against recursive calls to allocation functions during
    /// TLS initialization.
    static mut INIT: bool = false;

    #[cfg(all(feature = "nightly", target_thread_local))]
    #[thread_local]
    /// A "cached" pointer to the thread-local allocator. This is set after initialization and
    /// set to null out prior to destruction.
    static mut PTR: *mut ElfMalloc<PA, TieredSizeClasses<ObjectAlloc<PA>>> = ptr::null_mut();

    #[cfg_attr(feature = "cargo-clippy", allow(inline_always))]
    #[inline(always)]
    /// Signal that initialization has begun. Note the use of a fall-back/stable method uses an
    /// atomic integer. This is painfully slow.
    fn init_begin() {
        #[cfg(feature = "nightly")]
        #[cfg(target_thread_local)]
        unsafe {
            INIT = true;
        }
        #[cfg(feature = "nightly")]
        #[cfg(not(target_thread_local))]
        {
            INITIALIZING.fetch_add(1, Ordering::Relaxed);
        }

        #[cfg(not(feature = "nightly"))]
        {
            INITIALIZING.fetch_add(1, Ordering::Relaxed);
        }
    }

    #[cfg_attr(feature = "cargo-clippy", allow(inline_always))]
    #[inline(always)]
    /// The inverse of `init_begin`.
    fn init_end() {
        #[cfg(feature = "nightly")]
        #[cfg(target_thread_local)]
        unsafe {
            INIT = false;
        }
        #[cfg(feature = "nightly")]
        #[cfg(not(target_thread_local))]
        {
            INITIALIZING.fetch_sub(1, Ordering::Relaxed);
        }

        #[cfg(not(feature = "nightly"))]
        {
            INITIALIZING.fetch_sub(1, Ordering::Relaxed);
        }
    }

    #[cfg_attr(feature = "cargo-clippy", allow(inline_always))]
    #[inline(always)]
    /// Check if we are in a recursive call to an allocation function.
    fn is_initializing() -> bool {
        #[cfg(feature = "nightly")]
        #[cfg(target_thread_local)]
        unsafe { INIT }
        #[cfg(feature = "nightly")]
        #[cfg(not(target_thread_local))]
        {
            INITIALIZING.load(Ordering::Relaxed) > 0
        }

        #[cfg(not(feature = "nightly"))]
        {
            INITIALIZING.load(Ordering::Relaxed) > 0
        }
    }


    #[derive(Clone)]
    /// A wrapper like `DynamicAllocator` in the parent module.
    ///
    /// The reason we have a wrapper is for this module's custom `Drop` implementation, mentioned
    /// in the module documentation.
    struct GlobalAllocator {
        inner: Option<ElfMalloc<PA, TieredSizeClasses<ObjectAlloc<PA>>>>,
    }
    unsafe impl Send for GlobalAllocator {}

    /// The type of the global instance of the allocator.
    ///
    /// This is used to create handles for TLS-stored `GlobalAllocator`s.
    struct GlobalAllocProvider {
        inner: Option<ElfMalloc<PA, TieredSizeClasses<ObjectAlloc<PA>>>>,
    }

    // We need sync to have the global allocator reference live for new threads to clone. This is
    // safe only because ElfMalloc (and PageAlloc, and TieredSizeClasses) have thread-safe clone
    // methods.
    unsafe impl Sync for GlobalAllocProvider {}
    impl GlobalAllocProvider {
        fn new() -> GlobalAllocProvider {
            GlobalAllocProvider { inner: Some(ElfMalloc::new()) }
        }
    }

    /// The type for messages sent to the background thread. These can either be arrays of size
    /// classes to be cleaned up (in the case of thread destruction) or pointers to be freed (in
    /// the case of a recursive call to `free`).
    enum Husk {
        Array(ElfMalloc<PA, TieredSizeClasses<ObjectAlloc<PA>>>),
        #[allow(dead_code)]
        Ptr(*mut u8),
        #[allow(dead_code)]
        Slag(*mut u8),
    }

    unsafe impl Send for Husk {}

    impl Drop for GlobalAllocator {
        fn drop(&mut self) {
            fn with_chan<F: FnMut(&Sender<Husk>)>(mut f: F) {
                #[cfg(feature = "nightly")]
                {
                    #[cfg(target_thread_local)]
                    {
                        unsafe { PTR = ptr::null_mut() };
                    }
                    LOCAL_DESTRUCTOR_CHAN
                        .try_with(|chan| f(chan))
                        .unwrap_or_else(|_| {
                            let chan = DESTRUCTOR_CHAN.lock().unwrap().clone();
                            f(&chan);
                        })
                }
                #[cfg(not(feature = "nightly"))]
                {
                    let chan = DESTRUCTOR_CHAN.lock().unwrap().clone();
                    f(&chan);
                }
            }
            // XXX: Why this check?
            //
            // We have found that for some reason, this destructor can be called more than once on
            // the same value. This could be a peculiarity of the TLS implementation, or it could
            // be a bug in the code here. Regardless; without this check there are some cases in
            // which this benchmark drops Arc-backed data-structures multiple times, leading to
            // segfaults either here or in the background thread.
            if self.inner.is_none() {
                return;
            }
            unsafe {
                with_chan(|chan| {
                    let dyn = ptr::read(self.inner.as_ref().unwrap());
                    let _ = chan.send(Husk::Array(dyn));
                });
                ptr::write(&mut self.inner, None);
            };
        }
    }

    pub unsafe fn get_layout(item: *mut u8) -> (usize /* size */, usize /* alignment */) {
        let m_block = match get_type(item) {
            // TODO(ezrosent): this duplicates some work..
            AllocType::SmallSlag | AllocType::Large => {
                LOCAL_ELF_HEAP.with(|h| {
                    (*h.get())
                        .inner
                        .as_ref()
                        .unwrap()
                        .small_pages
                        .backing_memory()
                })
            }
            AllocType::BigSlag => {
                LOCAL_ELF_HEAP.with(|h| {
                    (*h.get())
                        .inner
                        .as_ref()
                        .unwrap()
                        .large_pages
                        .backing_memory()
                })
            }
        };
        super::elfmalloc_get_layout(m_block, item)
    }

    fn new_handle() -> GlobalAllocator {
        GlobalAllocator {
            inner: Some(ELF_HEAP.inner.as_ref().expect("heap uninitialized").clone()),
        }
    }

    impl Drop for GlobalAllocProvider {
        fn drop(&mut self) {
            mem::forget(self.inner.take());
        }
    }

    lazy_static! {
        static ref ELF_HEAP: GlobalAllocProvider = GlobalAllocProvider::new();
        static ref DESTRUCTOR_CHAN: Mutex<Sender<Husk>> = {
            // Background thread code: block on a channel waiting for memory reclamation messages
            // (Husks).
            let (sender, receiver) = channel();
            thread::spawn(move || unsafe {
                let mut local_alloc = new_handle();
                loop {
                    if let Ok(msg) = receiver.recv() {
                        let msg: Husk = msg;
                        match msg {
                            Husk::Array(alloc) => mem::drop(DynamicAllocator(alloc)),
                            Husk::Ptr(p) => local_alloc.inner.as_mut().unwrap().free(p),
                            Husk::Slag(s) => dirty_slag(s),
                        }
                        continue
                    }
                    mem::forget(local_alloc);
                    return;
                }
            });
            Mutex::new(sender)
        };
    }

    lazy_static!{
        // only used on stable nightly or targets where thread-local is not supported
        #[allow(unused_variables)]
        pub static ref INITIALIZING: AtomicUsize = AtomicUsize::new(0);
    }

    thread_local! {
        static LOCAL_DESTRUCTOR_CHAN: Sender<Husk> =
            DESTRUCTOR_CHAN.lock().unwrap().clone();
        static LOCAL_ELF_HEAP: UnsafeCell<GlobalAllocator> = UnsafeCell::new(new_handle());
    }

    pub unsafe fn alloc(size: usize) -> *mut u8 {
        #[cfg(feature = "nightly")]
        #[cfg(target_thread_local)]
        {
            if likely(!PTR.is_null()) {
                return (*PTR).alloc(size);
            }
        }
        trace!("fallback alloc({:?})", size);
        if is_initializing() {
            return super::large_alloc::alloc(size);
        }
        init_begin();
        let res = alloc_inner(size);
        init_end();
        res
    }

    unsafe fn alloc_inner(size: usize) -> *mut u8 {
        #[cfg(feature = "nightly")]
        {
            LOCAL_ELF_HEAP
                .try_with(|h| {
                    let res = (*h.get()).inner.as_mut().unwrap().alloc(size);
                    PTR = (*h.get()).inner.as_mut().unwrap() as *const _ as *mut _;
                    res
                })
                .unwrap_or_else(|_| super::large_alloc::alloc(size))
        }

        #[cfg(not(feature = "nightly"))]
        {
            LOCAL_ELF_HEAP.with(|h| (*h.get()).inner.as_mut().unwrap().alloc(size))
        }
    }

    pub unsafe fn realloc(item: *mut u8, new_size: usize) -> *mut u8 {
        aligned_realloc(item, new_size, mem::size_of::<usize>())
    }

    pub unsafe fn aligned_realloc(item: *mut u8, new_size: usize, new_alignment: usize) -> *mut u8 {
        #[cfg(feature = "nightly")]
        {
            if likely(!PTR.is_null()) {
                (*PTR).realloc(item, new_size, new_alignment);
            }
        }
        assert!(!is_initializing(), "realloc can't be called recursively");
        init_begin();
        let res = LOCAL_ELF_HEAP.with(|h| {
            (*h.get()).inner.as_mut().unwrap().realloc(
                item,
                new_size,
                new_alignment,
            )
        });
        init_end();
        res
    }

    pub unsafe fn free(item: *mut u8) {
        #[cfg(feature = "nightly")]
        {
            #[cfg(target_thread_local)]
            #[thread_local]
            {
                if likely(!PTR.is_null()) {
                    return (*PTR).free(item);
                }
            }
            LOCAL_ELF_HEAP
                .try_with(|h| (*h.get()).inner.as_mut().unwrap().free(item))
                .unwrap_or_else(|_| match get_type(item) {
                    AllocType::Large => {
                        super::large_alloc::free(item);
                    }
                    AllocType::SmallSlag | AllocType::BigSlag => {
                        let chan = DESTRUCTOR_CHAN.lock().unwrap().clone();
                        let _ = chan.send(Husk::Ptr(item));
                    }
                });
        }
        #[cfg(not(feature = "nightly"))]
        {
            LOCAL_ELF_HEAP.with(|h| (*h.get()).inner.as_mut().unwrap().free(item))
        }
    }
}

/// A trait encapsulating the notion of an array of size classes for an allocator.
pub trait AllocMap<T>
    where Self: Sized
{
    /// The type used to index size classes.
    type Key;

    /// Create and initialize the map.
    fn init<F: FnMut(Self::Key) -> T>(start: Self::Key, n_classes: usize, f: F) -> Self {
        Self::init_conserve(start, n_classes, f).1
    }

    /// Create and initialize the map, handing back ownership of the constructor.
    fn init_conserve<F: FnMut(Self::Key) -> T>(start: Self::Key,
                                               n_classes: usize,
                                               f: F)
                                               -> (F, Self);

    /// Get an unchecked raw pointer to the class corresponding to `k`.
    unsafe fn get_raw(&self, k: Self::Key) -> *mut T;

    /// Get an unchecked reference to the class corresponding to `k`.
    #[cfg_attr(feature = "cargo-clippy", allow(inline_always))]
    #[inline(always)]
    unsafe fn get(&self, k: Self::Key) -> &T {
        &*self.get_raw(k)
    }

    /// Get an unchecked mutable reference to the class corresponding to `k`.
    #[cfg_attr(feature = "cargo-clippy", allow(inline_always))]
    #[inline(always)]
    unsafe fn get_mut(&mut self, k: Self::Key) -> &mut T {
        &mut *self.get_raw(k)
    }

    /// Iterate over the map's contents.
    ///
    /// This is used to clean up the contents of the map.
    fn foreach<F: Fn(*mut T)>(&self, f: F);

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
struct TieredSizeClasses<T> {
    // When compiling for the C API, the minimum alignment is 16 on Mac and 64-bit Windows.
    #[cfg(any(not(feature = "c-api"), not(any(target_os = "macos", all(windows, target_pointer_width = "64")))))]
    word_objs: Option<T>,
    small_objs: Multiples<T>,
    medium_objs: PowersOfTwo<T>,
}

impl<T> AllocMap<T> for TieredSizeClasses<T> {
    type Key = usize;
    fn init_conserve<F: FnMut(usize) -> T>(start: usize, n_classes: usize, f: F) -> (F, Self) {
        let n_small_classes = n_classes / 2;
        let n_medium_classes = n_classes - n_small_classes;
        let (f2, small_classes) = Multiples::init_conserve(start, n_small_classes, f);
        let (mut f3, medium_classes) =
            PowersOfTwo::init_conserve(small_classes.max_key() + 1, n_medium_classes, f2);
        let word_objs = f3(8);
        (f3,
         TieredSizeClasses {
             // When compiling for the C API, the minimum alignment is 16 on Mac and 64-bit Windows.
                #[cfg(any(not(feature = "c-api"), not(any(target_os = "macos", all(windows, target_pointer_width = "64")))))]
             word_objs: Some(word_objs),
             small_objs: small_classes,
             medium_objs: medium_classes,
         })
    }

    unsafe fn get_raw(&self, n: usize) -> *mut T {
        // When compiling for the C API, the minimum alignment is 16 on Mac and 64-bit Windows.
        #[cfg(any(not(feature = "c-api"), not(any(target_os = "macos", all(windows, target_pointer_width = "64")))))]
        {
            if n <= 8 {
                self.word_objs.as_ref().unwrap() as *const _ as *mut T
            } else if n <= self.small_objs.max_key() {
                self.small_objs.get_raw(n)
            } else {
                self.medium_objs.get_raw(n)
            }
        }

        #[cfg(all(feature = "c-api", any(target_os = "macos", all(windows, target_pointer_width = "64"))))]
        {
            if n <= self.small_objs.max_key() {
                self.small_objs.get_raw(n)
            } else {
                self.medium_objs.get_raw(n)
            }
        }
    }

    #[inline]
    fn max_key(&self) -> usize {
        self.medium_objs.max_key()
    }

    fn foreach<F: Fn(*mut T)>(&self, f: F) {
        if let Some(r) = self.word_objs.as_ref() {
            f(r as *const _ as *mut T);
        }
        self.small_objs.foreach(&f);
        self.medium_objs.foreach(f);
    }
}

// Once this can be a type parameter, it should be.
pub const MULTIPLE: usize = 16;

/// An array of size classes where sizes are multiples of 16.
pub struct Multiples<T> {
    starting_size: usize,
    max_size: usize,
    pub classes: TypedArray<T>,
}

impl<T: Clone> Clone for Multiples<T> {
    fn clone(&self) -> Self {
        Multiples::init(self.starting_size,
                        self.classes.len(),
                        |size| unsafe { self.get(size).clone() })
    }
}

/// Round up to the closest multiple of 16 greater than or equal to `n`.
#[inline]
fn round_up(n: usize) -> usize {
    (n + (MULTIPLE - 1)) & !(MULTIPLE - 1)
}

impl<T> AllocMap<T> for Multiples<T> {
    type Key = usize;
    fn init_conserve<F: FnMut(usize) -> T>(start: usize, n_classes: usize, mut f: F) -> (F, Self) {
        debug_assert!(n_classes >= 1);
        let starting_size = round_up(start);
        let res = Multiples {
            starting_size: starting_size,
            max_size: n_classes * MULTIPLE + starting_size - MULTIPLE,
            classes: TypedArray::new(n_classes),
        };
        let mut cur_size = res.starting_size;
        for p in res.classes.iter() {
            unsafe {
                ptr::write(p, f(cur_size));
            }
            cur_size += MULTIPLE;
        }
        debug_assert_eq!(res.max_size, cur_size - MULTIPLE);
        (f, res)
    }

    #[cfg_attr(feature = "cargo-clippy", allow(inline_always))]
    #[inline(always)]
    unsafe fn get_raw(&self, n: usize) -> *mut T {
        let class = round_up(n);
        debug_assert!(class <= self.max_size);
        self.classes
            .get((round_up(n) - self.starting_size) / MULTIPLE)
    }

    #[inline]
    fn max_key(&self) -> usize {
        self.max_size
    }

    fn foreach<F: Fn(*mut T)>(&self, f: F) {
        for class in self.classes.iter() {
            f(class)
        }
    }
}

/// Size classes that are just the powers of two.
///
/// This is useful mostly for testing purposes: it is a very simple implementation, but it can also
/// be rather wasteful.
pub struct PowersOfTwo<T> {
    starting_size: usize,
    max_size: usize,
    pub classes: TypedArray<T>,
}

impl<T: Clone> Clone for PowersOfTwo<T> {
    fn clone(&self) -> Self {
        PowersOfTwo::init(self.starting_size,
                          self.classes.len(),
                          |size| unsafe { self.get(size).clone() })
    }
}

impl Drop for DynamicAllocator {
    fn drop(&mut self) {
        self.0.allocs.foreach(|x| unsafe { ptr::drop_in_place(x) });
        unsafe {
            self.0.allocs.medium_objs.classes.destroy();
            self.0.allocs.small_objs.classes.destroy();
            ptr::write(&mut self.0.allocs.word_objs, None);
        }
    }
}

impl<T> PowersOfTwo<T> {
    fn new(start_from: usize, n_classes: usize) -> PowersOfTwo<T> {
        PowersOfTwo {
            starting_size: start_from.next_power_of_two(),
            max_size: 0, // currently uninitialized
            classes: TypedArray::new(n_classes),
        }
    }
}

impl<T> AllocMap<T> for PowersOfTwo<T> {
    type Key = usize;
    fn init_conserve<F: FnMut(Self::Key) -> T>(start: usize,
                                               n_classes: usize,
                                               mut f: F)
                                               -> (F, Self) {
        let mut res = Self::new(start, n_classes);
        let mut cur_size = res.starting_size;
        unsafe {
            for item in res.classes.iter() {
                ptr::write(item, f(cur_size));
                cur_size *= 2;
            }
        }
        res.max_size = cur_size / 2;
        (f, res)
    }

    #[cfg_attr(feature = "cargo-clippy", allow(inline_always))]
    #[inline(always)]
    unsafe fn get_raw(&self, k: usize) -> *mut T {
        debug_assert!(k <= self.max_size);
        let log = (k.next_power_of_two().trailing_zeros() -
                   self.starting_size.trailing_zeros()) as usize;
        debug_assert!(log < self.classes.len(),
                      "log={} len={}",
                      log,
                      self.classes.len());
        self.classes.get(log)
    }

    #[inline]
    fn max_key(&self) -> usize {
        self.max_size
    }

    fn foreach<F: Fn(*mut T)>(&self, f: F) {
        for class in self.classes.iter() {
            f(class)
        }
    }
}
/// A Dynamic memory allocator, instantiated with sane defaults for various `ElfMalloc` type
/// parameters.
#[derive(Clone)]
pub struct DynamicAllocator(ElfMalloc<PageAlloc<Source>, TieredSizeClasses<ObjectAlloc<PageAlloc<Source>>>>);

unsafe impl Send for DynamicAllocator {}

impl DynamicAllocator {
    pub fn new() -> Self {
        DynamicAllocator(ElfMalloc::new())
    }
    pub unsafe fn alloc(&mut self, size: usize) -> *mut u8 {
        self.0.alloc(size)
    }
    pub unsafe fn free(&mut self, item: *mut u8) {
        self.0.free(item)
    }

    pub unsafe fn realloc(&mut self, item: *mut u8, new_size: usize) -> *mut u8 {
        self.0.realloc(item, new_size, mem::size_of::<usize>())
    }

    pub unsafe fn aligned_realloc(&mut self,
                                  item: *mut u8,
                                  new_size: usize,
                                  new_alignment: usize)
                                  -> *mut u8 {
        self.0.realloc(item, new_size, new_alignment)
    }
}

// we default to using the `MagazineCache` here, as it performs better in general. There are some
// settings in which the `LocalCache` frontend is superior. Hence, we feature-gate this.
#[cfg(not(feature = "local_cache"))]
pub type ObjectAlloc<CA> = Lazy<MagazineCache<CA>>;
#[cfg(feature = "local_cache")]
pub type ObjectAlloc<CA> = Lazy<LocalCache<CA>>;

/// A Dynamic memory allocator, parmetrized on a particular `ObjectAlloc`, `CourseAllocator` and
/// `AllocMap`.
///
/// `ElfMalloc` encapsulates the logic of constructing and selecting object classes, as well as
/// delgating to the `large_alloc` module for large allocations. Most of the logic occurs in its
/// type parameters.
struct ElfMalloc<CA: CoarseAllocator, AM: AllocMap<ObjectAlloc<CA>>> {
    /// A cache of pages for all small allocations.
    small_pages: CA,
    /// A cache of pages for all medium allocations.
    large_pages: CA,
    /// An `AllocMap` of size classes of individual fixed-size object allocator.
    allocs: AM,
    /// The maximum size of a "non-large" object. Objects larger than `max_size` are allocated
    /// directly with mmap.
    max_size: usize,

    start_from: usize,
    n_classes: usize,
}

impl Default for DynamicAllocator {
    fn default() -> Self {
        Self::new()
    }
}

// TODO(ezrosent): move this to a type parameter when const generics are in.
const ELFMALLOC_PAGE_SIZE: usize = 2 << 20;

impl<M: MemorySource, D: DirtyFn>
    ElfMalloc<PageAlloc<M, D>, TieredSizeClasses<ObjectAlloc<PageAlloc<M, D>>>> {
    fn new() -> Self {
        let pa_large = PageAlloc::new(ELFMALLOC_PAGE_SIZE, 1 << 20, 8, AllocType::BigSlag);
        // The small pages are allocated in groups where the first page is aligned to
        // ELFMALLOC_PAGE_SIZE; this page will be stamped with AllocType::SmallSlag, allowing type
        // lookups to work as expected.
        // let pa_small = PageAlloc::new_aligned(128 << 10, 1 << 20, 8, ELFMALLOC_PAGE_SIZE, AllocType::SmallSlag);
        let pa_small = PageAlloc::new_aligned(
            256 << 10,
            1 << 20,
            8,
            ELFMALLOC_PAGE_SIZE,
            AllocType::SmallSlag,
        );
        Self::new_internal(0.6, pa_small, pa_large, 8, 25)
    }
}

#[inline]
unsafe fn round_to_page<T>(item: *mut T) -> *mut T {
    ((item as usize) & !(ELFMALLOC_PAGE_SIZE - 1)) as *mut T
}

/// We ensure that for every pointer returned from a call to `alloc`, rounding that pointer down to
/// a 2MiB boundary yields the location of an `AllocType`. This is enforced separately in the
/// `PageAlloc` code, the `large_alloc` code, and the `Slag` code.
///
/// All of this allows us to run elfmalloc with a full malloc-style interface without resorting to
/// any sort of global ownership check on the underlying `MemorySource`. This method thus breaks
/// our dependency on the `Creek`.
#[inline]
unsafe fn get_type(item: *mut u8) -> AllocType {
    *round_to_page(item.offset(-1) as *mut AllocType)
}

impl<M: MemorySource, D: DirtyFn, AM: AllocMap<ObjectAlloc<PageAlloc<M, D>>, Key = usize>> Clone
    for ElfMalloc<PageAlloc<M, D>, AM> {
    fn clone(&self) -> Self {
        let new_map = AM::init(self.start_from, self.n_classes, |size: usize| unsafe {
            self.allocs.get(size).clone()
        });
        ElfMalloc {
            small_pages: self.small_pages.clone(),
            large_pages: self.large_pages.clone(),
            allocs: new_map,
            max_size: self.max_size,
            start_from: self.start_from,
            n_classes: self.n_classes,
        }
    }
}


unsafe fn elfmalloc_get_layout<M: MemorySource>(m_block: &M, item: *mut u8) -> (usize, usize) {
    match get_type(item) {
        AllocType::SmallSlag | AllocType::BigSlag => {
            let meta = (*Slag::find(item, m_block.page_size())).get_metadata();
            (
                meta.object_size,
                if meta.object_size.is_power_of_two() {
                    meta.object_size
                } else {
                    mem::size_of::<usize>()
                },
            )
        }
        AllocType::Large => (large_alloc::get_size(item), mmap::page_size()),
    }
}

impl<M: MemorySource, D: DirtyFn, AM: AllocMap<ObjectAlloc<PageAlloc<M, D>>, Key = usize>>
    ElfMalloc<PageAlloc<M, D>, AM> {
    fn new_internal(
        // usable_size: usize,
        cutoff_factor: f64,
        pa_small: PageAlloc<M, D>,
        pa_large: PageAlloc<M, D>,
        start_from: usize,
        n_classes: usize,
    ) -> Self {
        use self::mmap::map;
        let mut meta_pointer = map(mem::size_of::<Metadata>() * n_classes) as *mut Metadata;
        let small_page_size = pa_small.backing_memory().page_size();
        let am = AM::init(start_from, n_classes, |size: usize| {
            let (u_size, pa, ty) = if size < small_page_size / 4 {
                (small_page_size, pa_small.clone(), AllocType::SmallSlag)
            } else {
                (
                    pa_large.backing_memory().page_size(),
                    pa_large.clone(),
                    AllocType::BigSlag,
                )
            };
            let m_ptr = meta_pointer;
            unsafe {
                meta_pointer = meta_pointer.offset(1);
                ptr::write(
                    m_ptr,
                    compute_metadata(
                        size,
                        pa.backing_memory().page_size(),
                        0,
                        cutoff_factor,
                        u_size,
                        ty,
                    ),
                );
            }
            let clean = PageCleanup::new(pa.backing_memory().page_size());
            // TODO(ezrosent); new_size(8) is a good default, but a better one would take
            // num_cpus::get() into account when picking this size, as in principle this will run
            // into scaling limits at some point.
            let params = (
                m_ptr,
                1 << 20,
                pa,
                RevocablePipe::new_size_cleanup(16, clean),
            );
            ObjectAlloc::new(params)
        });
        let max_size = am.max_key();
        ElfMalloc {
            small_pages: pa_small.clone(),
            large_pages: pa_large.clone(),
            allocs: am,
            max_size: max_size,
            start_from: start_from,
            n_classes: n_classes,
        }
    }

    unsafe fn get_page_size(&self, item: *mut u8) -> Option<usize> {
        match get_type(item) {
            AllocType::SmallSlag => Some(self.small_pages.backing_memory().page_size()),
            AllocType::BigSlag => Some(self.large_pages.backing_memory().page_size()),
            AllocType::Large => None,
        }
    }

    unsafe fn alloc(&mut self, bytes: usize) -> *mut u8 {
        if likely(bytes < self.max_size) {
            self.allocs.get_mut(bytes).alloc()
        } else {
            large_alloc::alloc(bytes)
        }
    }

    unsafe fn realloc(&mut self,
                      item: *mut u8,
                      mut new_size: usize,
                      new_alignment: usize)
                      -> *mut u8 {
        if item.is_null() {
            return self.alloc(new_size);
        }
        if new_size == 0 {
            self.free(item);
            return ptr::null_mut();
        }
        match self.get_page_size(item) {
            Some(page_size) => {
                let slag = &*Slag::find(item, page_size);
                let meta = slag.get_metadata();
                let old_size = meta.object_size;
                if old_size.is_power_of_two() && new_size <= old_size {
                    return item;
                }
                if new_alignment > mem::size_of::<usize>() && !new_size.is_power_of_two() &&
                    new_size <= self.max_size
                {
                    new_size = new_size.next_power_of_two();
                }
                let new_memory = self.alloc(new_size);
                ptr::copy_nonoverlapping(item, new_memory, meta.object_size);
                self.free(item);
                new_memory
            }
            None => {
                let size = large_alloc::get_size(item);
                if size >= new_size {
                    return item;
                }
                let new_memory = self.alloc(new_size);
                ptr::copy_nonoverlapping(item, new_memory, size);
                new_memory
            }
        }
    }

    unsafe fn free(&mut self, item: *mut u8) {
        match self.get_page_size(item) {
            Some(page_size) => {
                let slag = &*Slag::find(item, page_size);
                self.allocs.get_mut(slag.get_metadata().object_size).free(
                    item,
                )
            }
            None => large_alloc::free(item),
        };
    }
}

mod large_alloc {
    //! This module governs "large" allocations that are beyond the size of the largest size class
    //! of a dynamic allocator.
    //!
    //! Large allocations are implemented by mapping a region of memory of the indicated size, with
    //! an additional page of padding to store the size information.
    #[cfg(test)]
    use std::collections::HashMap;
    #[cfg(test)]
    use std::cell::RefCell;
    use std::ptr;
    use super::{ELFMALLOC_PAGE_SIZE, round_to_page};
    use super::super::alloc_type::AllocType;

    // For debugging, we keep around a thread-local map of pointers to lengths. This helps us
    // scrutinize if various header data is getting propagated correctly.
    #[cfg(test)]
    thread_local! {
        pub static SEEN_PTRS: RefCell<HashMap<*mut u8, usize>> = RefCell::new(HashMap::new());
    }
    use super::mmap::{map, unmap};

    #[repr(C)]
    #[derive(Copy, Clone)]
    struct AllocInfo {
        ty: AllocType,
        base: *mut u8,
        region_size: usize,
    }

    pub unsafe fn alloc(size: usize) -> *mut u8 {
        // TODO(ezrosent) round up to page size
        let region_size = size + ELFMALLOC_PAGE_SIZE;
        let mem = map(region_size);
        let res = mem.offset(ELFMALLOC_PAGE_SIZE as isize);
        let addr = get_commitment_mut(res);
        ptr::write(
            addr,
            AllocInfo {
                ty: AllocType::Large,
                base: mem,
                region_size: region_size,
            },
        );

        // begin extra debugging information
        debug_assert!(!mem.is_null());
        let upage: usize = 4096;
        debug_assert_eq!(mem as usize % upage, 0);
        debug_assert_eq!(res as usize % upage, 0);
        #[cfg(test)] SEEN_PTRS.with(|hs| hs.borrow_mut().insert(mem, region_size));
        // end extra debugging information
        res
    }

    pub unsafe fn free(item: *mut u8) {
        let (size, base_ptr) = get_commitment(item);

        // begin extra debugging information:
        #[cfg(debug_assertions)]
        {
            ptr::write_volatile(item, 10);
        }
        #[cfg(test)]
        {
            SEEN_PTRS.with(|hm| {
                let mut hmap = hm.borrow_mut();
                {
                    if let Some(len) = hmap.get(&base_ptr) {
                        assert_eq!(*len, size);
                    }
                }
                hmap.remove(&base_ptr);
            });
        }
        // end extra debugging information
        unmap(base_ptr, size);
    }

    pub unsafe fn get_size(item: *mut u8) -> usize {
        let (size, _) = get_commitment(item);
        size - ELFMALLOC_PAGE_SIZE
    }

    unsafe fn get_commitment(item: *mut u8) -> (usize, *mut u8) {
        let meta_addr = get_commitment_mut(item);
        let base_ptr = (*meta_addr).base;
        let size = (*meta_addr).region_size;
        (size, base_ptr)
    }

    unsafe fn get_commitment_mut(item: *mut u8) -> *mut AllocInfo {
        round_to_page(item.offset(-1) as *mut AllocInfo)
    }
}

#[cfg(test)]
mod tests {
    extern crate env_logger;
    use super::*;
    use std::ptr::{write_bytes, write_volatile};


    #[test]
    fn layout_lookup() {
        fn test_and_free<F: Fn(usize, usize)>(inp: usize, tester: F) {
            unsafe {
                let obj = global::alloc(inp);
                let (size, align) = global::get_layout(obj);
                tester(size, align);
                global::free(obj);
            }
        }

        test_and_free(8, |size, align| assert_eq!((size, align), (8, 8)));
        test_and_free(24, |size, align| {
            assert!(size >= 24);
            assert!(align >= 8);
        });
        test_and_free(512, |size, align| assert_eq!((size, align), (512, 512)));
        test_and_free(4 << 20, |size, align| {
            assert_eq!((size, align), (4 << 20, mmap::page_size()))
        });
    }

    #[test]
    fn general_alloc_basic_global_single_threaded() {
        let _ = env_logger::init();
        for size in ((1 << 13) - 8)..((1 << 13) + 1) {
            unsafe {
                let item = global::alloc(size * 8);
                write_volatile(item, 10);
                global::free(item);
            }
        }
    }

    #[test]
    fn general_alloc_basic_clone_single_threaded() {
        let _ = env_logger::init();
        let da_c = DynamicAllocator::new();
        let mut da = da_c.clone();
        for size in ((1 << 13) - 8)..((1 << 13) + 1) {
            unsafe {
                let item = da.alloc(size * 8);
                write_volatile(item, 10);
                da.free(item);
            }
        }
    }

    #[test]
    fn general_alloc_basic_global_many_threads() {
        let _ = env_logger::init();
        use std::thread;

        const N_THREADS: usize = 32;
        let mut threads = Vec::with_capacity(N_THREADS);
        for t in 0..N_THREADS {
            threads.push(thread::Builder::new()
                             .name(t.to_string())
                             .spawn(move || {
                for size in 1..(1 << 13) {
                    // ((1 << 9) + 1)..((1 << 18) + 1) {
                    unsafe {
                        let item = global::alloc(size * 8);
                        write_volatile(item, 10);
                        global::free(item);
                    }
                    if size * 8 >= (1 << 20) {
                        return;
                    }
                }
            })
                             .unwrap());
        }

        for t in threads {
            t.join().expect("threads should exit successfully")
        }
    }

    #[test]
    fn general_alloc_large_ws_global_many_threads() {
        let _ = env_logger::init();
        use std::thread;

        const N_THREADS: usize = 32;
        let mut threads = Vec::with_capacity(N_THREADS);
        for t in 0..N_THREADS {
            threads.push(thread::Builder::new()
                             .name(t.to_string())
                             .spawn(move || unsafe {
                for _ in 0..2 {
                    let ptrs: Vec<*mut u8> = (0..(1 << 20)).map(|_| global::alloc(8)).collect();
                    for p in ptrs {
                        global::free(p);
                    }
                }
            })
                             .unwrap());
        }

        for t in threads {
            t.join().expect("threads should exit successfully")
        }
    }

    #[test]
    fn realloc_basic() {
        let _ = env_logger::init();
        use std::thread;
        const N_THREADS: usize = 32;
        let alloc = DynamicAllocator::new();
        let mut threads = Vec::with_capacity(N_THREADS);
        for t in 0..N_THREADS {
            let mut da = alloc.clone();
            threads.push(thread::Builder::new()
                             .name(t.to_string())
                             .spawn(move || for size in 1..(1 << 13) {
                                        unsafe {
                                            let item = da.alloc(size * 8);
                                            write_bytes(item, 0xFF, size * 8);
                                            let new_item =
                                                da.aligned_realloc(item,
                                                                   size * 16,
                                                                   if size % 2 == 0 {
                                                                       8
                                                                   } else {
                                                                       size.next_power_of_two()
                                                                   });
                                            write_bytes(item.offset(size as isize * 8),
                                                        0xFE,
                                                        size * 8);
                                            da.free(new_item);
                                        }
                                        if size * 8 >= (1 << 20) {
                                            return;
                                        }
                                    })
                             .unwrap());
        }

        for t in threads {
            t.join().expect("threads should exit successfully")
        }
    }

    #[test]
    fn general_alloc_basic_clone_many_threads() {
        let _ = env_logger::init();
        use std::thread;

        const N_THREADS: usize = 32;
        let alloc = DynamicAllocator::new();
        let mut threads = Vec::with_capacity(N_THREADS);
        for t in 0..N_THREADS {
            let mut da = alloc.clone();
            threads.push(thread::Builder::new()
                             .name(t.to_string())
                             .spawn(move || {
                for size in 1..(1 << 13) {
                    // ((1 << 9) + 1)..((1 << 18) + 1) {
                    unsafe {
                        let item = da.alloc(size * 8);
                        write_bytes(item, 0xFF, size * 8);
                        da.free(item);
                    }
                    if size * 8 >= (1 << 20) {
                        return;
                    }
                }
            })
                             .unwrap());
        }

        for t in threads {
            t.join().expect("threads should exit successfully")
        }
    }

    #[test]
    fn all_sizes_one_thread() {
        let _ = env_logger::init();
        for size in 1..((1 << 21) + 1) {
            unsafe {
                let item = global::alloc(size);
                write_volatile(item, 10);
                global::free(item);
                if size + 2 > 1 << 20 {
                    return;
                }
            }
        }
    }
}
