//! This module is an attempt at craeting a variant of elfmalloc that takes advantage of the extra
//! information that the `Alloc` trait provides.

use super::alloc::allocator::{Alloc, AllocErr, Layout};

use super::general::{Multiples, PowersOfTwo, ObjectAlloc, MULTIPLE, AllocMap};
use super::slag::{PageAlloc, MemorySource, Metadata, RevocablePipe, compute_metadata,
                  CoarseAllocator};
use super::utils::{mmap, Lazy};

use std::cmp;
use std::mem;
use std::ptr;


/// A `MemorySource` that just calls mmap. It still maintains that all pages returned by `carve`
/// are aligned to their size.
#[derive(Copy, Clone)]
pub struct MmapSource {
    page_size: usize,
}

unsafe impl Send for MmapSource {}

impl MemorySource for MmapSource {
    fn new(page_size: usize) -> MmapSource {
        MmapSource { page_size: page_size.next_power_of_two() }
    }
    fn page_size(&self) -> usize {
        self.page_size
    }

    fn carve(&self, npages: usize) -> Option<*mut u8> {
        trace!("carve({:?})", npages);
        // faster mod for power-of-2 sizes.
        fn mod_size(x: usize, n: usize) -> usize {
            x & (n - 1)
        }

        // There is a faster path available when our local page size is less than or equal to the
        // system one.
        let system_page_size = mmap::page_size();
        if self.page_size <= system_page_size {
            return mmap::fallible_map(npages * self.page_size);
        }
        // We want to return pages aligned to our page size, which is larger than the
        // system page size. As a result, we want to allocate an extra page to guarantee a slice of
        // the memory that is aligned to the larger page size.
        let target_size = npages * self.page_size;
        let req_size = target_size + self.page_size;
        mmap::fallible_map(req_size).and_then(|mem| {
            let mem_num = mem as usize;

            debug_assert_eq!(mod_size(mem_num, system_page_size), 0);

            // region at the end that is not needed.
            let rem1 = mod_size(mem_num, self.page_size);
            // region at the beginning that is not needed.
            let rem2 = self.page_size - rem1;
            unsafe {
                let res = mem.offset(rem2 as isize);
                debug_assert_eq!(mod_size(res as usize, self.page_size), 0);
                if rem1 > 0 {
                    mmap::unmap(res.offset(target_size as isize), rem1);
                }
                if rem2 > 0 {
                    mmap::unmap(mem, rem2);
                }
                Some(res)
            }
        })
    }
}

/// An `Alloc` impl that takes advantage of size information at the call-site to more efficiently
/// handle larger allocations.
#[derive(Clone)]
pub struct ElfMalloc<M: MemorySource> {
    small: Multiples<ObjectAlloc<PageAlloc<M>>>,
    large: PowersOfTwo<Lazy<PageAlloc<M>>>,
}

/// Following the structure of the `general` module, we keep the underlying `ElfMalloc` struct with
/// a trivial drop method, as drop needs to be specialized when the allocator is and isn't used in
/// thread local storage.
///
/// `OwnedElfMalloc` is the standard version, where the destructor reclaims any cached memory.
#[derive(Clone)]
pub struct OwnedElfMalloc<M: MemorySource>(pub ElfMalloc<M>);


impl<M: MemorySource> ::std::ops::Deref for OwnedElfMalloc<M> {
    type Target = ElfMalloc<M>;
    fn deref(&self) -> &ElfMalloc<M> {
        &self.0
    }
}

impl<M: MemorySource> ::std::ops::DerefMut for OwnedElfMalloc<M> {
    fn deref_mut(&mut self) -> &mut ElfMalloc<M> {
        &mut self.0
    }
}

unsafe impl<M: MemorySource + Send> Send for ElfMalloc<M> {}
unsafe impl<M: MemorySource + Send> Send for OwnedElfMalloc<M> {}

impl<M: MemorySource> ElfMalloc<M> {
    unsafe fn destroy(&mut self) {
        self.small.foreach(|x| ptr::drop_in_place(x));
        self.large.foreach(|x| ptr::drop_in_place(x));
        self.small.classes.destroy();
        self.large.classes.destroy();
    }
}

impl<M: MemorySource> Drop for OwnedElfMalloc<M> {
    fn drop(&mut self) {
        unsafe {
            self.0.destroy();
        }
    }
}


macro_rules! case_analyze {
    ($self:expr, $layout:expr, small $small:expr; medium $medium:expr; large $large:expr;) => {
        {
            if $layout.size() <= $self.small.max_key() {
                $small
            } else if $layout.size() <= $self.large.max_key() {
                $medium
            } else {
                $large
            }
        }
    }
}

unsafe impl<M: MemorySource> Alloc for ElfMalloc<M> {
    #[inline(always)]
    unsafe fn alloc(&mut self, l: Layout) -> Result<*mut u8, AllocErr> {
        trace!("alloc({:?})", l);
        case_analyze!(
            self,
            l,
            small Ok(
                self.small
                    .get_mut(if l.align() > mem::size_of::<usize>() {
                        l.size().next_power_of_two()
                    } else {
                        l.size()
                    })
                    .alloc(),
            );
            medium Ok(self.large.get_mut(l.size()).alloc());
            large match mmap::fallible_map(l.size()) {
                Some(p) => Ok(p),
                None => Err(AllocErr::Exhausted { request: l }),
            };
        )
    }

    #[inline(always)]
    unsafe fn dealloc(&mut self, item: *mut u8, l: Layout) {
        trace!("dealloc({:?}, {:?})", item, l);
        case_analyze!(
            self,
            l,
            small self.small.get_mut(if l.align() > mem::size_of::<usize>() {
                        l.size().next_power_of_two()
                    } else {
                        // round up to nearest MULTIPLE
                        (l.size() + (MULTIPLE - 1)) & !(MULTIPLE - 1)
                    }).free(item);
            medium self.large.get_mut(l.size()).free(item, false);
            large mmap::unmap(item, l.size());)
    }

    #[inline(always)]
    fn usable_size(&self, l: &Layout) -> (usize, usize) {
        trace!("usable_size({:?})", l.clone());
        (
            l.size(),
            case_analyze!(
            self,
            l,
            small if l.align() > mem::size_of::<usize>() {
                l.size().next_power_of_two()
            } else {
                l.size()
            };
            medium l.size().next_power_of_two();
            large l.size();),
        )
    }
}

unsafe impl<M: MemorySource> Alloc for OwnedElfMalloc<M> {
    unsafe fn alloc(&mut self, l: Layout) -> Result<*mut u8, AllocErr> {
        self.0.alloc(l)
    }

    unsafe fn dealloc(&mut self, p: *mut u8, l: Layout) {
        self.0.dealloc(p, l)
    }

    fn usable_size(&self, l: &Layout) -> (usize, usize) {
        self.0.usable_size(l)
    }
}

pub struct ElfMallocBuilder {
    page_size: usize,
    target_pa_size: usize,
    reuse_threshold: f64,
    max_object_size: usize,
    small_pipe_size: usize,
    large_pipe_size: usize,
}

impl Default for ElfMallocBuilder {
    fn default() -> ElfMallocBuilder {
        ElfMallocBuilder {
            page_size: 32 << 10,
            // very large -- this effectively turns the feature off
            target_pa_size: 1 << 30,
            reuse_threshold: 0.6,
            max_object_size: 2 << 20,
            small_pipe_size: 8,
            large_pipe_size: 16,
        }
    }
}

impl ElfMallocBuilder {
    pub fn page_size(&mut self, page_size: usize) -> &mut ElfMallocBuilder {
        self.page_size = cmp::max(mmap::page_size(), page_size);
        self
    }
    pub fn target_pa_size(&mut self, target_pa_size: usize) -> &mut ElfMallocBuilder {
        self.target_pa_size = target_pa_size;
        self
    }
    pub fn reuse_threshold(&mut self, reuse_threshold: f64) -> &mut ElfMallocBuilder {
        self.reuse_threshold = reuse_threshold;
        self
    }
    pub fn max_object_size(&mut self, max_object_size: usize) -> &mut ElfMallocBuilder {
        self.max_object_size = max_object_size;
        self
    }
    pub fn small_pipe_size(&mut self, small_pipe_size: usize) -> &mut ElfMallocBuilder {
        self.small_pipe_size = small_pipe_size;
        self
    }
    pub fn large_pipe_size(&mut self, large_pipe_size: usize) -> &mut ElfMallocBuilder {
        self.large_pipe_size = large_pipe_size;
        self
    }

    pub fn build<M: MemorySource>(&self) -> ElfMalloc<M> {
        let pa = PageAlloc::<M>::new(self.page_size, self.target_pa_size, self.large_pipe_size);
        let n_small_classes = (self.page_size / 4) / MULTIPLE;
        assert!(n_small_classes > 0);
        let mut meta_pointers = mmap::map(mem::size_of::<Metadata>() * n_small_classes) as
            *mut Metadata;
        let small_classes = Multiples::init(MULTIPLE, n_small_classes, |size: usize| {
            let meta = meta_pointers;
            unsafe {
                meta_pointers = meta_pointers.offset(1);
                ptr::write(
                    meta,
                    compute_metadata(
                        size,
                        self.page_size,
                        0,
                        self.reuse_threshold,
                        self.page_size,
                    ),
                );
            }
            let params = (
                meta,
                usize::max_value(), /* no eager decommit */
                pa.clone(),
                RevocablePipe::new_size(self.small_pipe_size),
            );
            ObjectAlloc::new(params)
        });
        let next_size_class = (small_classes.max_key() + 1).next_power_of_two();
        let max_size = self.max_object_size.next_power_of_two();
        let n_classes = max_size.trailing_zeros() - next_size_class.trailing_zeros();
        let large_classes = PowersOfTwo::init(next_size_class, n_classes as usize, |size: usize| {
            Lazy::<PageAlloc<M>>::new((size, self.target_pa_size, self.large_pipe_size))
        });
        debug_assert!(small_classes.max_key().is_power_of_two());
        ElfMalloc {
            small: small_classes,
            large: large_classes,
        }
    }

    pub fn build_owned<M: MemorySource>(&self) -> OwnedElfMalloc<M> {
        OwnedElfMalloc(self.build())
    }
}

pub use self::global::{DynamicAlloc, SharedAlloc, new_owned_handle};

mod global {
    //! This module provides an interface to global instances of allocators in the parent module.
    //! Two interfaces are provides. All allocations from either of these allocators share the same
    //! global data-structures.
    //!
    //! # `DynamicAlloc`
    //!
    //! This allocator is a complete instance of the `ElfMalloc` frontend. When its destructor is
    //! called, any cached memory is returned to the global pool of memory. A `DynamicAlloc` can be
    //! embedded in a long-lived data-structure and grant its allocations some degree of isolation
    //! from other components of the current thread. Embedding the allocator in this manner also
    //! ensure that allocations and frees are cached correctly even when the allocator is moved to
    //! a different thread.
    //!
    //! # `SharedAlloc`
    //!
    //! This allocator stores a single frontend per thread in thread-local storage (TLS). It
    //! therefore allows different data-structures to share a single frontend. In experiments with
    //! a custom `Vec` implementation, this leads to serious performance wins.
    use super::*;

    use std::intrinsics::unlikely;
    use std::sync::mpsc::{channel, Sender};
    use std::sync::Mutex;
    use std::thread;
    use std::cell::UnsafeCell;

    type InnerAlloc = ElfMalloc<MmapSource>;

    pub type DynamicAlloc = OwnedElfMalloc<MmapSource>;

    /// A global handle that can hand out thread-local handles to the allocator.
    struct ElfCloner(InnerAlloc);

    impl ElfCloner {
        fn new_handle(&self) -> DynamicAlloc {
            OwnedElfMalloc(self.0.clone())
        }
    }
    unsafe impl Sync for ElfCloner {}

    /// Construct a new `DynamicAlloc`.
    pub fn new_owned_handle() -> DynamicAlloc {
        GLOBAL_HANDLE.new_handle()
    }

    lazy_static! {
        static ref GLOBAL_HANDLE: ElfCloner = ElfCloner(ElfMallocBuilder::default()
                                                        .page_size(32 << 10)
                                                        .build());

        /// We still have a crossbeam dependency, which means that we may have to reclaim a
        /// thread's cached memory after it is destroyed. See the comments in `general::global` for
        /// more context on why this is necessary.
        static ref BACKUP_CLEAN: Mutex<Sender<DynamicAlloc>> = {
            let (sender, receiver) = channel();
            thread::spawn(move || loop {
                if let Ok(msg) = receiver.recv() {
                    mem::drop(msg);
                }
            });
            Mutex::new(sender)
        };
    }

    #[thread_local]
    static mut RUST_PTR: *mut InnerAlloc = ptr::null_mut();

    /// A newtype wrapper around `ElfMalloc<MmapSource>` that sends its contents to the background
    /// thread in its destructor.
    struct ElfMallocTLS(InnerAlloc);

    impl Drop for ElfMallocTLS {
        fn drop(&mut self) {
            unsafe {
                let _ = BACKUP_CLEAN.lock().unwrap().send(OwnedElfMalloc(
                    ptr::read(&mut self.0),
                ));
            }
        }
    }

    thread_local! {
        static LOCAL_RUSTMALLOC: UnsafeCell<ElfMallocTLS> =
            UnsafeCell::new(ElfMallocTLS(GLOBAL_HANDLE.0.clone()));
    }

    macro_rules! with_instance {

        ($ptr:ident, $exp:expr) => {
            with_instance!($ptr, $exp, panic!("TLS in invalid state"))
        };

        ($ptr:ident, $exp:expr, $else:expr) => {
            if unlikely(RUST_PTR.is_null()) {
                LOCAL_RUSTMALLOC.try_with(|uc| {
                    let $ptr: &mut InnerAlloc = &mut (*uc.get()).0;
                    let res = $exp;
                    RUST_PTR = $ptr;
                    res
                }).unwrap_or_else(|_| {
                    $else
                })
            } else {
                let $ptr = &mut *RUST_PTR;
                $exp
            }
        };

    }

    /// A ZST for routing allocations through the thread-local handle.
    #[derive(Clone)]
    pub struct SharedAlloc;

    unsafe impl Alloc for SharedAlloc {
        unsafe fn alloc(&mut self, l: Layout) -> Result<*mut u8, AllocErr> {
            with_instance!(r_ptr, r_ptr.alloc(l))
        }
        unsafe fn dealloc(&mut self, p: *mut u8, l: Layout) {
            with_instance!(r_ptr, r_ptr.dealloc(p, l))
        }
        fn usable_size(&self, l: &Layout) -> (usize, usize) {
            unsafe { with_instance!(r_ptr, r_ptr.usable_size(l)) }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::mem;
    use std::ptr;
    use std::thread;

    #[test]
    fn basic_alloc_functionality() {
        let word_size = mem::size_of::<usize>();
        let mut alloc = ElfMallocBuilder::default().build_owned::<MmapSource>();
        let layouts: Vec<_> = (word_size..(8 << 10))
            .map(|size| {
                Layout::from_size_align(size * 128, word_size).unwrap()
            })
            .collect();
        unsafe {
            let mut ptrs = Vec::new();
            for l in &layouts {
                let p = alloc.alloc(l.clone()).expect("alloc should not fail") as *mut usize;
                ptr::write_volatile(p, !0);
                ptrs.push(p);
            }
            for (ptr, l) in ptrs.into_iter().zip(layouts.into_iter()) {
                alloc.dealloc(ptr as *mut u8, l);
            }
        }
    }

    fn multi_threaded_alloc_test(layouts: Vec<Layout>) {
        const N_THREADS: usize = 64;
        let alloc = ElfMallocBuilder::default().build_owned::<MmapSource>();

        let mut threads = Vec::new();
        for _ in 0..N_THREADS {
            let mut my_alloc = alloc.clone();
            let my_layouts = layouts.clone();
            threads.push(thread::spawn(move || unsafe {
                for _ in 0..2 {
                    let mut ptrs = Vec::new();
                    for l in &my_layouts {
                        let p = my_alloc.alloc(l.clone()).expect("alloc should not fail") as
                            *mut usize;
                        ptr::write_volatile(p, !0);
                        ptrs.push(p);
                    }
                    for (ptr, l) in ptrs.into_iter().zip(my_layouts.iter()) {
                        my_alloc.dealloc(ptr as *mut u8, (*l).clone());
                    }
                }
            }))
        }
        for t in threads.into_iter() {
            t.join().expect("threads should not fail")
        }
    }

    #[test]
    fn many_threads_many_sizes() {
        let word_size = mem::size_of::<usize>();
        multi_threaded_alloc_test(
            (word_size..(2 << 10))
                .map(|size| {
                    Layout::from_size_align(size * 1024, word_size).unwrap()
                })
                .collect(),
        );
    }

    #[test]
    fn large_ws_small_size() {
        multi_threaded_alloc_test(
            (1..(8 << 10))
                .map(|_| Layout::from_size_align(64, 64).unwrap())
                .collect(),
        );
    }

    #[test]
    fn large_ws_large_size() {
        multi_threaded_alloc_test(
            (1..(1 << 8))
                .map(|_| Layout::from_size_align(512 << 10, 64).unwrap())
                .collect(),
        );
    }
}
