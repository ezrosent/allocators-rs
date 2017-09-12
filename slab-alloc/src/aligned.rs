// Copyright 2017 the authors. See the 'Copyright and license' section of the
// README.md file at the top-level directory of this repository.
//
// Licensed under the Apache License, Version 2.0 (the LICENSE-APACHE file) or
// the MIT license (the LICENSE-MIT file) at your option. This file may not be
// copied, modified, or distributed except according to those terms.

//! A slab that uses alignment to map objects to slabs.
//!
//! This module provides a slab implementation similar to Bonwick's original small slabs, but which
//! relies on variable-size blocks of memory instead of fixed-size pages (page-sized blocks can be
//! allocated, making Bonwick's original small slabs a special case of aligned slabs).
//!
//! The core idea behind aligned slabs, as with small slabs, is to back each slab with a chunk of
//! memory whose alignment is equal to its size (thus the size is always a power of two). This
//! makes it so that, given a pointer to an allocated object, figuring out which slab that object
//! was allocated from is a trivial matter of masking off the insignificant lower-order bits,
//! keeping only the bits needed to identify the containing slab. Compare this with the large slab
//! algorithm, which requires keeping an allocator-global hash table mapping object pointers to
//! their containing slabs.
//!
//! This implementation also differs from the original Bonwick algorithm in that it keeps track of
//! available objects in a stack of pointers rather than in a linked list. For more details on this
//! approach to keeping track of available objects, see the `stack` module.

extern crate alloc;
extern crate object_alloc;

use {OBJECTS_PER_SLAB, PAGE_SIZE, stack};
use stack::{SlabHeader, Layout};
use init::InitSystem;
use self::alloc::allocator;
use self::object_alloc::UntypedObjectAlloc;

pub struct ConfigData;

impl stack::ConfigData for ConfigData {
    fn ptr_to_slab(&self, slab_size: usize, ptr: *mut u8) -> *mut SlabHeader {
        let slab = ptr as usize & !(slab_size - 1);
        debug_assert_eq!(slab % slab_size, 0);
        slab as *mut SlabHeader
    }
}

pub type System<A> = stack::System<A, ConfigData>;

impl<A: UntypedObjectAlloc> System<A> {
    pub fn new(layout: allocator::Layout, alloc: A) -> Option<System<A>> {
        if let Some((slab_layout, _)) = Layout::for_slab_size(layout, alloc.layout().size()) {
            Some(Self::from_config_data(ConfigData, slab_layout, alloc))
        } else {
            None
        }
    }
}

pub fn backing_size_for<I: InitSystem>(layout: &allocator::Layout) -> usize {
    struct PowerOfTwoIterator(usize);

    impl Iterator for PowerOfTwoIterator {
        type Item = usize;

        fn next(&mut self) -> Option<usize> {
            if (self.0 << 1) == 0 {
                // another double would overflow usize
                None
            } else {
                self.0 *= 2;
                Some(self.0)
            }
        }
    }

    let unused = |slab_size: usize| {
        Layout::for_slab_size(layout.clone(), slab_size).map(|(layout, unused)| {
                                                                 (layout.num_obj, unused)
                                                             })
    };

    // We guarantee that we never request aligned slabs smaller than a page (see the Documentation
    // on SlabAllocBuilder::build_backing), so we start off with an initial size of *PAGE_SIZE and
    // go up from there.
    ::util::size::choose_size(PowerOfTwoIterator(*PAGE_SIZE), unused, OBJECTS_PER_SLAB)
}
