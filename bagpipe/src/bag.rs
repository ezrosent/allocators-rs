// Copyright 2017-2018 the authors. See the 'Copyright and license' section of the
// README.md file at the top-level directory of this repository.
//
// Licensed under the Apache License, Version 2.0 (the LICENSE-APACHE file) or
// the MIT license (the LICENSE-MIT file) at your option. This file may not be
// copied, modified, or distributed except according to those terms.

//! Specification of best-effort bags and implementation for `crossbeam`
//! data-structures.
use super::crossbeam::sync::{TreiberStack, SegQueue, MsQueue};
use super::crossbeam_epoch::{Collector, Guard, Handle};
use std::sync::atomic::AtomicUsize;
use std::sync::Arc;

/// Ways that operations on a `SharedWeakBag` or `WeakBag` can fail.
///
/// We permit `push` and `pop` operations to exhibit transient failures.
pub enum PopStatus {
    Empty,
    TransientFailure,
}

pub type PopResult<T> = Result<T, PopStatus>;

/// A best-effort Bag data-structure.
///
/// As embodied in the `PopResult` definition, `try_pop` is permitted to
/// fail even if the bag in question is not empty.
pub trait SharedWeakBag where Self: Sized {
    type Item;

    /// Returns a new instance of the data-structure.
    fn new() -> Self;

    /// Attempts to push `it` onto the data-structure.
    ///
    /// If successful, `try_push` will return `true`.
    fn try_push(&self, guard: &Guard, it: Self::Item) -> Result<(), Self::Item>;

    /// Attempts to pop a value from the data-structure.
    ///
    /// There is no guaranteed ordering of popped values. This method
    /// may fail arbitrarily even if there are accessible values in the
    /// data-structure.
    fn try_pop(&self, guard: &Guard) -> PopResult<Self::Item>;

    /// A push operation that will not fail.
    ///
    /// The default implementation of `push` simply calls `try_push`
    /// in a loop. until it succeeds. Depending on the underlying
    /// data-structure this may loop infinitely under some
    /// circumstances.
    fn push(&self, guard: &Guard, it: Self::Item) {
        let mut cur_item = it;
        while let Err(old_item) = self.try_push(guard, cur_item) {
            cur_item = old_item
        }
    }

    /// A pop operation that will not fail.
    ///
    /// Same caveats apply to those of `push`.
    fn pop(&self, guard: &Guard) -> Option<Self::Item> {
        loop {
            return match self.try_pop(guard) {
                Ok(it) => Some(it),
                Err(PopStatus::Empty) => None,
                Err(PopStatus::TransientFailure) => continue,
            };
        }
    }

    fn debug(&self) {}
}

/// An `Arc`-style variant of `SharedWeakBag`.
///
/// This gives implementations the freedom of modifying mutable
/// local metadata. Any `SharedWeakBag` is also a `WeakBag` if
/// behind an `Arc`. Methods on `WeakBag` have the same semantics as
/// `SharedWeakbag` except that the `try...` methods are permitted to
/// modify any thread-local state.
pub trait WeakBag: Clone {
    // TODO(ezrosent): should we keep Clone here?
    type Item;

    fn try_push_mut(&mut self, Self::Item) -> Result<(), Self::Item>;
    fn try_pop_mut(&mut self) -> PopResult<Self::Item>;
    fn push_mut(&mut self, it: Self::Item) {
        // TODO(joshlf): Pin the WeakBag's GC for performance
        let mut cur_item = it;
        while let Err(old_item) = self.try_push_mut(cur_item) {
            cur_item = old_item
        }
    }
    fn pop_mut(&mut self) -> Option<Self::Item> {
        // TODO(joshlf): Pin the WeakBag's GC for performance
        loop {
            match self.try_pop_mut() {
                Ok(it) => break Some(it),
                Err(PopStatus::Empty) => break None,
                Err(PopStatus::TransientFailure) => {}
            }
        }
    }

    /// Add all items in `I` to the `WeakBag`.
    ///
    /// This allows data-structures to optimize bulk-add operations if
    /// possible.
    fn bulk_add<I: Iterator<Item = Self::Item>>(&mut self, i: I) {
        for it in i {
            self.push_mut(it)
        }
    }
}

pub struct ArcLike<B>{
    arc: Arc<B>,
    gc: Handle,
}

impl<B> Clone for ArcLike<B> {
    fn clone(&self) -> Self {
        ArcLike {
            arc: self.arc.clone(),
            gc: self.gc.clone(),
        }
    }
}

impl<B: SharedWeakBag> Default for ArcLike<B> {
    fn default() -> Self {
        ArcLike {
            arc: Arc::new(B::new()),
            gc: Collector::new().handle(),
        }
    }
}

impl<B: SharedWeakBag> WeakBag for ArcLike<B> {
    type Item = B::Item;

    fn try_push_mut(&mut self, it: Self::Item) -> Result<(), Self::Item> {
        self.arc.try_push(&self.gc.pin(), it)
    }

    fn try_pop_mut(&mut self) -> PopResult<Self::Item> {
        self.arc.try_pop(&self.gc.pin())
    }

    fn push_mut(&mut self, it: Self::Item) {
        self.arc.push(&self.gc.pin(), it)
    }

    fn pop_mut(&mut self) -> Option<Self::Item> {
        self.arc.pop(&self.gc.pin())
    }
}


/// Types that can revoke their membership in a `RevocableWeakBag`.
///
/// This is a fairly low-level interface; most of the time it should not be needed. There are also
/// some performance pitfalls in the way it is implemented in, e.g., the `FAAQueueLowLevel`
/// data-structure. In that case, it works by pointing the value of `handle` to the cell in which a
/// value is stored. Revocation is therefore simply a compare-and-swap operation on this value,
/// attempting to change it to the "poison" sentinel value. If this happens infrequently it is
/// likely fine. However, excessive calls to `revoke` will lead to `pop` operations slowing down
/// because they must skip over poisoned cells.
pub trait Revocable {
    /// A reference to an `AtomicUsize` value that can be used by a `revoke` implementation.
    ///
    /// The intended use for this is to set aside a word of memory in `Self` to hold a reference to
    /// its location in a `SharedWeakBag`. That way, the underlying data-structure can revoke
    /// membership with a single CAS.
    ///
    /// This interface is low-level; it may change depending on use and demand for queues with this
    /// feature.
    fn handle(&self) -> &AtomicUsize;
}


// This is a code smell. The reason why it is here is to allow for custom
// revocable types to be revocable even when they are stored as raw pointers in one of the "low
// level" bags.

impl<T: Revocable> Revocable for *mut T {
    fn handle(&self) -> &AtomicUsize {
        unsafe {
            self.as_ref()
                .expect("revocable impl dereferences raw pointers")
                .handle()
        }
    }
}

/// A `SharedWeakBag` that can attempt to revoke `push` operations.
pub trait RevocableWeakBag: SharedWeakBag
where
    Self::Item: Revocable,
{
    /// Attempt to remove `it` from the bag, returning `true` if successful.
    ///
    /// This operation is unsafe because the underlying implementation may assume that `it` is (or
    /// was) successfully pushed into the bag at some point.
    unsafe fn revoke(it: &Self::Item) -> bool;
}

// implement WeakBag for the stack and queues in crossbeam. Note that these don't have the full
// "try" semantics that we want, as they never fail. As a result, they should not be used in a
// `BagPipe`: everything will work, but

impl<T> SharedWeakBag for TreiberStack<T> {
    type Item = T;

    fn new() -> Self {
        Self::new()
    }

    fn try_push(&self, _guard: &Guard, t: T) -> Result<(), T> {
        self.push(t);
        Ok(())
    }

    fn try_pop(&self, _guard: &Guard) -> PopResult<T> {
        match self.pop() {
            Some(res) => Ok(res),
            None => Err(PopStatus::Empty),
        }
    }
}

impl<T> SharedWeakBag for SegQueue<T> {
    type Item = T;

    fn new() -> Self {
        Self::new()
    }

    fn try_push(&self, _guard: &Guard, t: T) -> Result<(), T> {
        self.push(t);
        Ok(())
    }

    fn try_pop(&self, _guard: &Guard) -> PopResult<T> {
        match self.try_pop() {
            Some(res) => Ok(res),
            None => Err(PopStatus::Empty),
        }
    }
}

impl<T> SharedWeakBag for MsQueue<T> {
    type Item = T;

    fn new() -> Self {
        Self::new()
    }

    fn try_push(&self, _guard: &Guard, t: T) -> Result<(), T> {
        self.push(t);
        Ok(())
    }

    fn try_pop(&self, _guard: &Guard) -> PopResult<T> {
        match self.try_pop() {
            Some(res) => Ok(res),
            None => Err(PopStatus::Empty),
        }
    }
}
