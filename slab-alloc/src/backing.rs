// don't conflict with the alloc module
extern crate alloc as crate_alloc;
extern crate object_alloc;

use self::object_alloc::UntypedObjectAlloc;

/// A pair of `UntypedObjectAlloc`s to provide memory for a slab allocator.
///
/// A `BackingAlloc` encapsulates a pair of allocator types that are used to provide the memory
/// needed to construct the slabs used by a slab allocator. Under the hood, slab allocators use
/// two different types of slabs - "aligned" slabs and "large" slabs - with different requirements
/// for backing memory. A `BackingAlloc` provides an allocator for each of the two slab types.
pub trait BackingAlloc {
    /// An allocator for aligned slabs.
    ///
    /// An `Aligned` allocator allocates the memory for aligned slabs. All memory allocated by this
    /// allocator must have an alignment which is equal to its size. An `Aligned` allocator must be
    /// capable of allocating page-sized blocks of memory. Allocated memory should be
    /// uninitialized.
    type Aligned: UntypedObjectAlloc;

    /// An allocator for large slabs.
    ///
    /// A `Large` allocator allocates the memory for large slabs. All memory allocated by this
    /// allocator must be page-aligned; a `Large` allocator will never be constructed with an
    /// object size smaller than the page size. Allocated memory should be uninitialized.
    type Large: UntypedObjectAlloc;
}

/// An `UntypedObjectAlloc` that uses arbitrary allocators.
mod alloc {
    extern crate alloc;
    extern crate object_alloc;
    use self::alloc::allocator::{Alloc, AllocErr, Layout};
    use self::object_alloc::{Exhausted, UntypedObjectAlloc};

    /// An `UntypedObjectAlloc` that uses an arbitrary allocator.
    #[derive(Clone)]
    pub struct AllocObjectAlloc<A: Alloc> {
        alloc: A,
        layout: Layout,
    }

    impl<A: Alloc> AllocObjectAlloc<A> {
        pub fn new(alloc: A, layout: Layout) -> AllocObjectAlloc<A> {
            AllocObjectAlloc { alloc, layout }
        }
    }

    unsafe impl<A: Alloc> UntypedObjectAlloc for AllocObjectAlloc<A> {
        fn layout(&self) -> Layout {
            self.layout.clone()
        }

        unsafe fn alloc(&mut self) -> Result<*mut u8, Exhausted> {
            match self.alloc.alloc(self.layout.clone()) {
                Ok(ptr) => Ok(ptr),
                Err(AllocErr::Exhausted { .. }) => Err(Exhausted),
                Err(AllocErr::Unsupported { details }) => {
                    unreachable!("unexpected unsupported alloc: {}", details)
                }
            }
        }

        unsafe fn dealloc(&mut self, ptr: *mut u8) {
            self.alloc.dealloc(ptr, self.layout.clone());
        }
    }
}

/// A `BackingAlloc` that uses the heap.
#[cfg(feature = "std")]
pub mod heap {
    extern crate alloc;
    use self::alloc::heap::{Heap, Layout};
    use super::alloc::AllocObjectAlloc;
    use super::BackingAlloc;
    use PAGE_SIZE;

    pub struct HeapBackingAlloc;

    impl BackingAlloc for HeapBackingAlloc {
        type Aligned = AllocObjectAlloc<Heap>;
        type Large = AllocObjectAlloc<Heap>;
    }

    pub fn max_align() -> usize {
        *PAGE_SIZE
    }

    pub fn new(layout: Layout) -> AllocObjectAlloc<Heap> {
        AllocObjectAlloc::new(Heap, layout)
    }
}
