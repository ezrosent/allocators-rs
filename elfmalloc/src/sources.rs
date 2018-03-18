// Copyright 2017-2018 the authors. See the 'Copyright and license' section of the
// README.md file at the top-level directory of this repository.
//
// Licensed under the Apache License, Version 2.0 (the LICENSE-APACHE file) or
// the MIT license (the LICENSE-MIT file) at your option. This file may not be
// copied, modified, or distributed except according to those terms.

//! Low-level data-structures for getting more memory from the system.
use std::sync::Arc;
use std::sync::atomic::{AtomicUsize, AtomicPtr, Ordering};
use std::mem;
use super::utils::{likely, mmap};

use alloc::allocator::{Alloc, Layout};
use mmap_alloc::MapAlloc;

/// A generator of chunks of memory providing an `sbrk`-like interface.
pub trait MemorySource
where
    Self: Clone,
{
    fn new(page_size: usize) -> Self;
    /// The smallest unit of memory that can be `carve`d.
    fn page_size(&self) -> usize;
    /// Return `npages` fresh pages from the `Creek`. Each of these pages is aligned to
    /// `page_size`.
    ///
    /// Currently, there is code (see the `Coalescer` in the `slag` module) that relies on fresh
    /// pages returned from `carve` to be filled with zeros.
    fn carve(&self, npages: usize) -> Option<*mut u8>;
}

/// A `MemorySource` that can tell which pointers lie in a region returned by `carve`.
pub trait MemoryBlock: MemorySource {
    /// Is `it` a pointer to somewhere in the block of memory.
    fn contains(&self, it: *mut u8) -> bool;
}


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
        // Even if self.page_size is larger than the system page size, that's OK
        // because we're using mmap-alloc's large-align feature, which allows
        // allocations to be aligned to any alignment, even larger than the page size.
        let layout = Layout::from_size_align(npages * self.page_size, self.page_size).unwrap();
        unsafe { MapAlloc::default().alloc(layout).ok() }
    }
}
