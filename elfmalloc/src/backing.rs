extern crate alloc;
extern crate object_alloc;
use self::alloc::allocator::Layout;
use self::object_alloc::{Exhausted, UntypedObjectAlloc};

// Things to consider:
// - Foo is basically UntypedObjectAlloc, but without dealloc, and with immutable alloc receiver
// - Bar is separate from Foo because PageCache doesn't require 'contains'

// TODO: Rename
pub unsafe trait Foo {
    fn layout(&self) -> Layout;
    unsafe fn alloc(&self) -> Result<*mut u8, Exhausted>;
}

pub unsafe trait Bar: Foo {
    // TODO: Should this be unsafe?
    unsafe fn contains(&self, ptr: *mut u8) -> bool;
}

pub trait BarBackedObjectAlloc: UntypedObjectAlloc
    where Self: Clone
{
    /// The concrete type representing backing memory for the allocator.
    type Backing: Bar;
    fn get_backing(&self) -> &Self::Backing;
}

pub mod page_cache {
    extern crate alloc;
    extern crate bagpipe;
    extern crate mmap_alloc;
    extern crate object_alloc;
    use self::alloc::allocator::Layout;
    use self::bagpipe::BagPipe;
    use self::bagpipe::bag::WeakBag;
    use self::bagpipe::queue::FAAQueueLowLevel;
    use self::mmap_alloc::MapAlloc;
    use self::object_alloc::{Exhausted, UntypedObjectAlloc};
    use super::{Foo, Bar, BarBackedObjectAlloc};

    pub unsafe trait PageAlloc: Foo {
        // TODO: What happens when you commit an already-committed page or uncommit an
        // already-uncommitted page?
        unsafe fn commit(&mut self, obj: *mut u8);
        unsafe fn uncommit(&mut self, obj: *mut u8);
    }

    unsafe impl Foo for MapAlloc {
        fn layout(&self) -> Layout {
            <MapAlloc as UntypedObjectAlloc>::layout(self)
        }

        unsafe fn alloc(&self) -> Result<*mut u8, Exhausted> {
            let mut self_ref = self;
            <&MapAlloc as UntypedObjectAlloc>::alloc(&mut self_ref)
        }
    }

    unsafe impl PageAlloc for MapAlloc {
        unsafe fn commit(&mut self, obj: *mut u8) {
            MapAlloc::commit(self, obj, <MapAlloc as UntypedObjectAlloc>::layout(self));
        }

        unsafe fn uncommit(&mut self, obj: *mut u8) {
            MapAlloc::uncommit(self, obj, <MapAlloc as UntypedObjectAlloc>::layout(self));
        }
    }

    #[derive(Clone)]
    pub struct PageCache<A: PageAlloc> {
        target_overhead: usize,
        alloc: A,
        // bagpipes of byte slices of size creek.page_size
        uncommitted: BagPipe<FAAQueueLowLevel<*mut u8>>,
        committed: BagPipe<FAAQueueLowLevel<*mut u8>>,
    }

    impl<A: PageAlloc> PageCache<A> {
        /// Creates a new `PageCache`.
        ///
        /// `new` creates a new `PageCache` which allocates from `alloc`. `target_overhead` is used
        /// as an approximate upper bound on the number of dirty objects that will be cached.
        pub fn new(alloc: A, target_overhead: usize) -> PageCache<A> {
            PageCache {
                target_overhead,
                alloc,
                uncommitted: BagPipe::new_size(2),
                committed: BagPipe::new_size(8),
            }
        }
    }

    unsafe impl<A: PageAlloc> UntypedObjectAlloc for PageCache<A> {
        fn layout(&self) -> Layout {
            self.alloc.layout()
        }

        unsafe fn alloc(&mut self) -> Result<*mut u8, Exhausted> {
            if let Ok(ptr) = self.committed.try_pop_mut() {
                Ok(ptr)
            } else if let Ok(ptr) = self.uncommitted.try_pop_mut() {
                self.alloc.commit(ptr);
                Ok(ptr)
            } else {
                self.alloc.alloc()
            }
        }

        unsafe fn dealloc(&mut self, obj: *mut u8) {
            let decommit = true; // TODO: This used to be a parameter. What is it?
            if decommit || self.committed.size_guess() >= self.target_overhead as isize {
                self.alloc.uncommit(obj);
                self.uncommitted.push_mut(obj);
            } else {
                self.committed.push_mut(obj);
            }
        }
    }

    impl<A: PageAlloc + Bar + Clone> BarBackedObjectAlloc for PageCache<A> {
        type Backing = A;
        fn get_backing(&self) -> &A {
            &self.alloc
        }
    }
}

pub mod creek {
    extern crate alloc;
    extern crate object_alloc;
    use self::alloc::allocator::Layout;
    use self::object_alloc::Exhausted;
    use std::sync::Arc;
    use std::sync::atomic::{AtomicPtr, AtomicUsize, Ordering};
    use std::mem;
    use super::{Foo, Bar};
    use super::page_cache::PageAlloc;

    /// Base address and size of a memory map.
    ///
    /// This could also just be a `*mut [u8]`, but having two fields is more explicit. We need a new
    /// type because the `Drop` implementation calls `unmap`.
    #[derive(Debug)]
    struct MapAddr(*mut u8, usize);

    impl Drop for MapAddr {
        fn drop(&mut self) {
            use utils::mmap::unmap;
            unsafe {
                unmap(self.0, self.1);
            }
        }
    }

    /// A large, contiguous, memory-mapped region of memory.
    ///
    /// A `Creek` can be seen as a very basic memory allocator that can hand back multiples of its
    /// `page_size`. While it does not implement `free`, the programmer can still call `uncommit` or
    /// `unmap` on pages that are returned from a `Creek`. In this module, the `Creek` is used to
    /// manage the (thread-safe) introduction of fresh (clean) pages into the rest of the allocator to
    /// be used.
    ///
    /// `Creek`s are initialized with a maximum size and never grow beyond that size. They are made
    /// with a particular idiom in mind in which a larger-than-physical-memory mapping is requested for
    /// the `Creek`, only a fraction of which is ever used by the running program (hence, ever backed
    /// by physical page frames).
    #[derive(Debug)]
    pub struct Creek {
        obj_size: usize,
        /// We use a `clone`-based interface in order to facilitate passing across threads and
        /// leveraging `Arc` to call `unmap`.
        map_info: Arc<MapAddr>,
        base: *mut u8,
        bump: AtomicPtr<AtomicUsize>,
    }

    unsafe impl Send for MapAddr {}
    unsafe impl Sync for MapAddr {}
    unsafe impl Send for Creek {}
    unsafe impl Sync for Creek {}

    macro_rules! check_bump {
        ($slf:expr) => {
            #[cfg(debug_assertions)]
            {
                let bump = $slf.bump.load(Ordering::Relaxed);
                debug_assert!(!bump.is_null());
            }
        };
    }

    impl Creek {
        /// Create a new `Creek` with objects of size `obj_size` total heap size of `heap_size`,
        /// optionally backed by huge pages.
        ///
        /// Object size and heap size should be powers of two. The allocator may want to reserve
        /// some pages for itself (or for alignment reasons), as a result it is a good idea to have
        /// `heap_size` be much larger than `obj_size`.
        pub fn new(obj_size: usize, heap_size: usize) -> Self {
            use utils::mmap::map;
            // lots of stuff breaks if this isn't true
            assert!(obj_size.is_power_of_two());
            assert!(obj_size > mem::size_of::<usize>());
            assert!(heap_size > obj_size * 3);
            // first, let's grab some memory;
            let orig_base = map(heap_size);
            let orig_addr = orig_base as usize;
            let (slush_addr, real_addr) = {
                // allocate some `slush` space at the beginning of the creek. This gives us space to
                // store the `bump` pointer. In the future, we may store more things in this slush
                // space as well.
                //
                // In addition, we must ensure that pages are aligned to their size.
                let base = if orig_addr == 0 {
                    orig_addr + obj_size
                } else if orig_addr % obj_size != 0 {
                    let rem = orig_addr % obj_size;
                    orig_addr + (obj_size - rem)
                } else {
                    orig_addr
                };
                (base as *mut u8, (base + obj_size) as *mut u8)
            };
            Creek {
                obj_size,
                map_info: Arc::new(MapAddr(orig_base, heap_size)),
                base: real_addr,
                bump: AtomicPtr::new(slush_addr as *mut AtomicUsize),
            }
        }
    }

    unsafe impl Foo for Creek {
        fn layout(&self) -> Layout {
            check_bump!(self);
            if cfg!(debug_assertions) {
                Layout::from_size_align(self.obj_size, self.obj_size).unwrap()
            } else {
                unsafe { Layout::from_size_align_unchecked(self.obj_size, self.obj_size) }
            }
        }

        unsafe fn alloc(&self) -> Result<*mut u8, Exhausted> {
            check_bump!(self);
            // TODO: Why do we load and then fetch_add separately?
            let new_bump = self.bump
                .load(Ordering::Relaxed)
                .as_ref()
                .unwrap()
                .fetch_add(1, Ordering::Relaxed);
            // TODO: When do we return Err(Exhausted)? How do we check?
            debug_assert!((new_bump * (self.obj_size + 1)) < self.map_info.1);
            Ok(self.base.offset((new_bump * self.obj_size) as isize))
        }
    }

    unsafe impl Bar for Creek {
        unsafe fn contains(&self, it: *mut u8) -> bool {
            check_bump!(self);
            let it_num = it as usize;
            let base_num = self.base as usize;
            it_num >= base_num && it_num < base_num + self.map_info.1
        }
    }

    unsafe impl PageAlloc for Creek {
        unsafe fn commit(&mut self, ptr: *mut u8) {
            unimplemented!()
        }
        unsafe fn uncommit(&mut self, ptr: *mut u8) {
            unimplemented!()
        }
    }

    impl Clone for Creek {
        fn clone(&self) -> Self {
            let bump = self.bump.load(Ordering::Relaxed);
            debug_assert!(!bump.is_null());
            Creek {
                obj_size: self.obj_size,
                map_info: self.map_info.clone(),
                base: self.base,
                bump: AtomicPtr::new(bump),
            }
        }
    }
}
