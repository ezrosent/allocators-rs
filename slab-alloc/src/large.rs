// So clippy doesn't complain that IllumOS isn't in tick marks
#![cfg_attr(feature = "cargo-clippy", allow(doc_markdown))]
//! A slab that uses an allocator-global hash table to map objects to slabs.
//!
//! This module provides a slab implementation similar to Bonwick's original large slabs. Unlike
//! aligned slabs, large slabs cannot rely on facts about memory layout to map object pointers to
//! the slabs from which they were allocated. Instead, an allocator-global hash table is
//! maintained. This is unfortunately somewhat slower than the aligned slab algorithm, but is
//! necessary for objects which are too large to fit in an aligned slab.
//!
//! This implementation differs significantly from the original Bonwick algorithm in its allocation
//! of metadata. In the Bonwick algorithm, each slab consists of a metadata structure which is
//! allocated separately from the buffer used to back objects. Additionally, each object has its
//! own separately-allocated metadata object (a `kmem_bufctl` in the IllumOS implementation). This
//! implementation instead takes a simpler approach, storing slab metadata in an inline header and
//! keeping track of available objects via a stack of pointers in the header - every large slab
//! consists of a single allocated region of memory. For more details on this stack-based approach
//! to keeping track of available objects, see the `stack` module.

extern crate alloc;
extern crate object_alloc;

use {PAGE_SIZE, PAGE_ALIGN_MASK, OBJECTS_PER_SLAB};
use stack;
use stack::{SlabHeader, Layout};
use init::InitSystem;
use util::ptrmap::*;
// use size::TypeSize;
use self::alloc::allocator;
use self::object_alloc::UntypedObjectAlloc;

pub struct ConfigData {
    pub map: Map<u8, SlabHeader>,
    map_by_page_addr: bool,
}

impl stack::ConfigData for ConfigData {
    fn post_alloc(&mut self, layout: &Layout, slab_size: usize, slab: *mut SlabHeader) {
        if self.map_by_page_addr {
            for i in 0..(slab_size / *PAGE_SIZE) {
                let page_ptr = ((slab as usize) + (i * *PAGE_SIZE)) as *mut u8;
                debug_assert_eq!(page_ptr as usize % *PAGE_SIZE, 0);
                self.map.insert(page_ptr, slab);
            }
        } else {
            for i in 0..layout.num_obj {
                let ptr = layout.nth_obj(slab, unsafe { (*slab).get_color() }, i);
                self.map.insert(ptr, slab);
            }
        }
    }

    fn pre_dealloc(&mut self, layout: &Layout, slab_size: usize, slab: *mut SlabHeader) {
        if self.map_by_page_addr {
            for i in 0..(slab_size / *PAGE_SIZE) {
                let page_ptr = ((slab as usize) + (i * *PAGE_SIZE)) as *mut u8;
                debug_assert_eq!(page_ptr as usize % *PAGE_SIZE, 0);
                self.map.delete(page_ptr);
            }
        } else {
            for i in 0..layout.num_obj {
                let ptr = layout.nth_obj(slab, unsafe { (*slab).get_color() }, i);
                self.map.delete(ptr);
            }
        }
    }

    fn ptr_to_slab(&self, _slab_size: usize, ptr: *mut u8) -> *mut SlabHeader {
        if self.map_by_page_addr {
            self.map
                .get(((ptr as usize) & *PAGE_ALIGN_MASK) as *mut u8)
        } else {
            self.map.get(ptr)
        }
    }
}

pub type System<A> = stack::System<A, ConfigData>;

pub const DEFAULT_MAP_SIZE: usize = 256;

impl<A: UntypedObjectAlloc> System<A> {
    pub fn new(layout: allocator::Layout, alloc: A) -> Option<System<A>> {
        if let Some((slab_layout, _)) =
            Layout::for_slab_size(layout.clone(), alloc.layout().size()) {
            let map_by_page_addr = layout.size() < *PAGE_SIZE;
            let map_key_align = if map_by_page_addr {
                *PAGE_SIZE
            } else {
                layout.size().next_power_of_two()
            };
            Some(Self::from_config_data(ConfigData {
                                            map: Map::new(DEFAULT_MAP_SIZE, map_key_align),
                                            map_by_page_addr: map_by_page_addr,
                                        },
                                        slab_layout,
                                        alloc))
        } else {
            None
        }
    }
}

pub fn backing_size_for<I: InitSystem>(layout: &allocator::Layout) -> usize {
    struct PageIterator(usize);

    impl Iterator for PageIterator {
        type Item = usize;

        fn next(&mut self) -> Option<usize> {
            let (next, _) = self.0.overflowing_add(*PAGE_SIZE);
            if next > self.0 {
                self.0 = next;
                Some(next)
            } else {
                None
            }
        }
    }

    let unused = |slab_size: usize| {
        Layout::for_slab_size(layout.clone(), slab_size).map(|(layout, unused)| {
                                                                 (layout.num_obj, unused)
                                                             })
    };

    // pick a reasonable lower bound on slab size to avoid wasting unnecessary work on slab sizes
    // that are too small to accomodate even a single object; for large object sizes, this can save
    // a considerable amount of work (e.g., for 128M objects, counting up from 1 page would take
    // ~32K iterations)
    let init_size = if layout.size() % *PAGE_SIZE == 0 {
        layout.size()
    } else {
        // round down to a multiple of the page size
        (layout.size() / *PAGE_SIZE) * *PAGE_SIZE
    };

    ::util::size::choose_size(PageIterator(init_size), unused, OBJECTS_PER_SLAB)
}

#[cfg(not(feature = "use-stdlib-hashmap"))]
#[cfg(test)]
mod tests {
    extern crate alloc;

    use {DefaultInitSystem, SizedSlabAlloc};
    use init::DefaultInitializer;
    use self::alloc::allocator::Layout;

    fn test_hash_table_bucket_distribution<T: Default>() {
        for i in 0..4 {
            use backing::heap;
            use sysconf::page::pagesize;

            let layout = Layout::new::<T>();
            let backing_layout = Layout::from_size_align(pagesize(), pagesize()).unwrap();
            let mut alloc =
                SizedSlabAlloc::new(DefaultInitSystem::<T>::new(DefaultInitializer::new()),
                                    layout.clone(),
                                    super::System::new(layout, heap::get_large(backing_layout))
                                        .unwrap());
            let mut ptrs = Vec::new();

            let size = super::DEFAULT_MAP_SIZE << i;
            for _ in 0..size {
                ptrs.push(alloc.alloc().unwrap());
            }
            let buckets = alloc.slab_system.data.map.map.dump_by_bucket();
            for p in ptrs {
                alloc.dealloc(p);
            }

            eprintln!("Size: {}", size);
            for (i, b) in buckets.iter().enumerate() {
                eprintln!("{:>3}: {:?}", i, b);
            }
            eprintln!("");
        }
    }

    macro_rules! make_test_hash_table_bucket_distribution {
        ($name:ident, $type:ty) => (
            #[test]
            #[ignore]
            fn $name() {
                test_hash_table_bucket_distribution::<$type>();
            }
        );
    }

    call_for_all_types_prefix!(make_test_hash_table_bucket_distribution,
                               test_hash_table_bucket_distribution);
}
