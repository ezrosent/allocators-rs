// Copyright 2017-2018 the authors. See the 'Copyright and license' section of the
// README.md file at the top-level directory of this repository.
//
// Licensed under the Apache License, Version 2.0 (the LICENSE-APACHE file) or
// the MIT license (the LICENSE-MIT file) at your option. This file may not be
// copied, modified, or distributed except according to those terms.

//! Implements the `BagPipe` data structure, along with its core components.
//!
//! A `BagPipe` is a concurrent pool data-structure optimized for
//! throughput and not much else. The core idea is to have a large
//! number of concurrent queues and stacks, along with a way of
//! load-balancing them in a coordination-free way that avoids wasting
//! resources and keeping contention low.
//!
//! # Example
//! For general-purpose use, the `BagPipe` has a somewhat low-level
//! interface. Here is some simple example usage:
//!
//! ```rust,ignore
//! let bp = BagPipe::<GeneralYC<T>>::new();
//! for _ in 0..NUM_THREADS {
//!     let mut my_bp = bp.clone();
//!     thread::spawn(move || {
//!         // ... do work
//!         // loop until push is successful
//!         my_bp.push_mut()
//!         // ... more work
//!         if let PopResult::There(item) = my_bp.try_pop_mut() {
//!             // use item
//!         }
//!     });
//! }
//! ```
//!
//! If you are passing a word-sized type, it is possible to reduce
//! allocation overhead by storing the data in-line. To do this,
//! replace `GeneralYC` with `YangCrummeyQueue` in the type parameter
//! for `BagPipe`: this will switch out the underlying backing
//! data-structure.
//!
//! The API currently supports `try...` versions of methods,
//! allowing data-structures to signal lack of progress due to high
//! contention. It also provides `push` and `pop` methods that will loop
//! until they succeed (or do something more intelligent).
//!
//! # Guarantees
//!
//! The data-structures given here are all non-blocking. The `try`
//! methods using `YangCrummeyQueue` and `GeneralYC` will return in a
//! bounded number of steps, but there is no guarantee they will succeed
//! except if they execute in isolation (i.e. Obstruction Freedom when
//! called in a loop). In constrast, those using `FAAArrayQueue` have a
//! lock-free progress guarantee. In general, a `BagPipe` inherits its
//! progress guarantees from its underlying `SharedWeakBag`, but it may
//! return arbitrary values with respect to their ordering guarantees.
//!
//! `BagPipe`'s emptiness check is not currently linearizable, but I
//! believe it is still serializable. In other words, it is possible
//! to re-order the execution history of the data-structure to respect
//! program order, but calls returning `Empty` may be re-ordered to a
//! time before or after the real execution time of the operation. A
//! marginally slower linearizable emptiness check would not be
//! difficult to engineer, and it will hopefully be added to the API
//! soon.

extern crate crossbeam;
extern crate crossbeam_epoch;
extern crate num_cpus;
use std::marker::PhantomData;
use std::sync::Arc;
use std::sync::atomic::{AtomicUsize, AtomicIsize, Ordering, fence};
use bag::{WeakBag, SharedWeakBag, RevocableWeakBag, Revocable, PopResult, PopStatus};
use crossbeam::mem::CachePadded;
use crossbeam_epoch::{Collector, Guard, Handle};
use std::mem;

pub mod queue;
pub mod bag;

#[cfg(feature = "prime_schedules")]
mod primes;

// Counters for the `size_guess` protocol.
const THRESHOLD_DIFF: isize = 4;
const N_COUNTERS: usize = 4;

pub trait BagCleanup {
    type Item;
    fn cleanup_all<'a, B: SharedWeakBag<Item = Self::Item> + 'a, I: Iterator<Item = &'a B>>(&self, guard: &Guard, it: I) {
        for bag in it {
            while let Some(p) = bag.pop(guard) {
                self.cleanup(p);
            }
        }
    }

    fn cleanup(&self, item: Self::Item);
}

#[derive(Copy, Clone)]
pub struct DummyCleanup<T>(PhantomData<T>);

impl<T> Default for DummyCleanup<T> {
    fn default() -> DummyCleanup<T> {
        DummyCleanup(PhantomData)
    }
}

impl<T> BagCleanup for DummyCleanup<T> {
    type Item = T;

    #[inline(always)]
    fn cleanup_all<'a, B: SharedWeakBag<Item = Self::Item> + 'a, I: Iterator<Item = &'a B>>(
        &self,
        _guard: &Guard,
        _it: I,
    ) {
    }
    #[inline(always)]
    fn cleanup(&self, _item: Self::Item) {}
}

/// A concurrent bag data-structure built from sharding requests over
/// other bags.
///
/// `BagPipe` implements both `SharedWeakBag` and `WeakBag`. Using this
/// as a `SharedWeakBag` tends to perform worse. Note that this should
/// never be used with `Arc<BagPipe>`, which will be much slower and
/// have an increased failure rate.
pub struct BagPipe<B: SharedWeakBag, Clean>
where
    Clean: BagCleanup<Item = B::Item>,
{
    pipes: Arc<BagPipeState<B, Clean>>,
    gc_handle: Handle,
    offset: usize,
    stride: usize,
    push_failures: usize,
    pop_failures: usize,
    cur_diff: isize,
}

impl<B: SharedWeakBag, Clean: BagCleanup<Item = B::Item>> Drop for BagPipe<B, Clean> {
    fn drop(&mut self) {
        if self.cur_diff != 0 {
            self.push_diff();
        }
    }
}

impl<B: SharedWeakBag, Clean: BagCleanup<Item = B::Item>> Clone for BagPipe<B, Clean> {
    fn clone(&self) -> Self {
        #[cfg(feature="prime_schedules")]
        let offset = {
            primes::get(self.pipes.all_refs.fetch_add(1, Ordering::Relaxed) + 1)
        };
        #[cfg(not(feature="prime_schedules"))]
        let offset = {
            let seed = self.pipes.all_refs.fetch_add(1, Ordering::Relaxed) + 1;
            seed * 2 + 1
        };
        BagPipe {
            pipes: self.pipes.clone(),
            gc_handle: self.pipes.gc.handle(),
            offset: offset & (self.pipes.pipes.len() - 1),
            stride: offset,
            push_failures: 0,
            pop_failures: 0,
            cur_diff: 0,
        }
    }
}

impl<B: SharedWeakBag, Clean: BagCleanup<Item = B::Item>> BagPipe<B, Clean> {
    pub fn new_size_cleanup(size: usize, clean: Clean) -> Self {
        #[cfg(feature="prime_schedules")]
        let offset = primes::get(1);
        #[cfg(not(feature="prime_schedules"))]
        let offset = 1;
        debug_assert!(size > 0);

        let pipes = Arc::new(BagPipeState::new_size(size, clean));
        let gc_handle = pipes.gc.handle();

        BagPipe {
            pipes,
            gc_handle,
            offset,
            stride: offset,
            push_failures: 0,
            pop_failures: 0,
            cur_diff: 0,
        }
    }

    fn push_diff(&mut self) {
        unsafe {
            self.pipes
                .counters
                .get_unchecked(self.offset % N_COUNTERS)
                .fetch_add(self.cur_diff, Ordering::Release);
        }
    }

    fn propagate_diff(&mut self, d: isize) {
        self.cur_diff += d;
        let thresh = THRESHOLD_DIFF;
        if self.cur_diff >= thresh || self.cur_diff <= -thresh {
            self.push_diff();
            self.cur_diff = 0;
        }
    }

    /// Return a guess of the current `BagPipe` size.
    ///
    /// This is implemented in a way that seeks to reduce overhead as
    /// much as possible. Currently there are 4 `AtomicIsize` counters
    /// in the global `BagPipeState` struct; one of these is updated
    /// when a thread accumulates a local diff (i.e. net pushes or pops)
    /// greater than a certain small constant. A thread querying the
    /// size of the `BagPipe` then simply sums these counters.
    ///
    /// Given this algorithm there are no consistency guarantees on
    /// this counter, not even eventual consistency. A counter is
    /// updated upon drop, so one "consistency guarantee" is that of
    /// a very weak quiescent consistency when "quiesce" means all
    /// threads that have pushed or popped from the data-structure have
    /// relinquished a handle on it. Note that this term usually refers
    /// to a sufficiently long period of inactivity.
    pub fn size_guess(&self) -> isize {
        use std::cmp;
        let mut total = 0;
        for ctr in &self.pipes.counters {
            total += ctr.load(Ordering::Acquire);
        }
        cmp::max(0, total)
    }
}

impl<B: SharedWeakBag, Clean: BagCleanup<Item = B::Item> + Default> Default for BagPipe<B, Clean> {
    fn default() -> Self {
        Self::new()
    }
}

impl<B: SharedWeakBag, Clean: BagCleanup<Item = B::Item> + Default> BagPipe<B, Clean> {
    /// Create a new `BagPipe` with `size` pipes.
    pub fn new_size(size: usize) -> Self {
        Self::new_size_cleanup(size, Clean::default())
    }

    pub fn new() -> Self {
        #[cfg(feature="prime_schedules")]
        let offset = primes::get(1);
        #[cfg(not(feature="prime_schedules"))]
        let offset = 1;

        let pipes = Arc::new(BagPipeState::new(Clean::default()));
        let gc_handle = pipes.gc.handle();

        BagPipe {
            pipes,
            gc_handle,
            offset,
            stride: offset,
            push_failures: 0,
            pop_failures: 0,
            cur_diff: 0,
        }
    }
}

impl<B: SharedWeakBag, Clean: BagCleanup<Item = B::Item>> WeakBag for BagPipe<B, Clean> {
    type Item = B::Item;

    fn try_push_mut(&mut self, it: Self::Item) -> Result<(), Self::Item> {
        let guard = self.gc_handle.pin();
        match self.pipes.try_push_internal(
            &guard,
            it,
            self.offset,
            self.stride,
            self.push_failures + 1,
            false,
        ) {
            Ok(_) => {
                self.push_failures >>= 1;
                self.propagate_diff(1);
                Ok(())
            }
            Err(item) => {
                self.offset += self.stride;
                self.push_failures += 1;
                Err(item)
            }
        }
    }

    fn try_pop_mut(&mut self) -> PopResult<Self::Item> {
        let guard = self.gc_handle.pin();
        let res = self.pipes.try_pop_internal(
            &guard,
            self.offset,
            self.stride,
            (self.pop_failures * 2) + 1,
        );
        match res {
            Err(PopStatus::TransientFailure) => {
                self.offset += self.stride;
                self.pop_failures += 1;
                Err(PopStatus::TransientFailure)
            }
            Err(PopStatus::Empty) => {
                self.pop_failures >>= 1;
                Err(PopStatus::Empty)
            }
            Ok(item) => {
                self.pop_failures >>= 1;
                self.propagate_diff(-1);
                Ok(item)
            }
        }
    }

    fn push_mut(&mut self, it: Self::Item) {
        let guard = self.gc_handle.pin();
        if let Err(it) = self.try_push_mut(it) {
            match self.pipes.try_push_internal(
                &guard,
                it,
                self.offset,
                self.stride,
                self.push_failures + 1,
                true,
            ) {
                Ok(true) => {
                    self.push_failures >>= 1;
                }
                Ok(false) => {
                    self.offset += self.stride;
                    self.push_failures += 1;
                }
                Err(_) => unreachable!(),
            }
            self.propagate_diff(1);
        }
    }

    fn bulk_add<I: Iterator<Item = Self::Item>>(&mut self, iter: I) {
        let guard = self.gc_handle.pin();

        let mut cur_index = self.offset;
        let p_len = self.pipes.pipes.len();
        let mut n_iters = 0;
        for item in iter {
            let mut it = item;
            loop {
                cur_index &= p_len - 1;
                let res = unsafe { self.pipes.pipes.get_unchecked(cur_index).try_push(&guard, it) };
                cur_index += self.stride;
                match res {
                    Ok(_) => {
                        n_iters += 1;
                        break;
                    }
                    Err(old_item) => {
                        it = old_item;
                    }
                }
            }
        }
        self.propagate_diff(n_iters)
    }
}

impl<B: RevocableWeakBag, Clean: BagCleanup<Item = B::Item>> BagPipe<B, Clean>
where
    B::Item: Revocable,
{
    /// Attempt to revoke `it` from membership in the `BagPipe`.
    ///
    /// This is simply an implementation of portions of the `Revocable`
    /// trait for a `BagPipe`.  We don't implement the trait here
    /// because it inherits from SharedWeakBag and BagPipes have
    /// unexpected behavior when used without the _mut() methods. This
    /// problem could be solved if we had "or" trait inheritance, but
    /// that could be more trouble than its worth :-)
    pub unsafe fn revoke(it: &B::Item) -> bool {
        B::revoke(it)
    }
}

struct BagPipeState<B: SharedWeakBag, Clean: BagCleanup<Item = B::Item>> {
    all_refs: AtomicUsize,
    counters: [CachePadded<AtomicIsize>; N_COUNTERS],
    pipes: Vec<B>,
    clean: Clean,
    gc: Collector,
}

impl<B: SharedWeakBag, Clean: BagCleanup<Item = B::Item>> Drop for BagPipeState<B, Clean> {
    fn drop(&mut self) {
        // Since we have exclusive access to self, we have exclusive access
        // to self.clean, which in turn means that we don't actually need
        // to worry about synchronization. Thus, a dummy guard is fine.
        let guard = unsafe { ::crossbeam_epoch::unprotected() };
        self.clean.cleanup_all(&guard, self.pipes.iter());
    }
}

impl<B: SharedWeakBag, Clean: BagCleanup<Item = B::Item>> BagPipeState<B, Clean> {
    // Creates a new `BagPipeState` with `sz` pipes, rounded up to the
    // next power of two.
    pub fn new_size(sz: usize, clean: Clean) -> Self {
        let len = sz.next_power_of_two();
        let mut res = BagPipeState {
            all_refs: AtomicUsize::new(1),
            counters: unsafe { mem::transmute([[0 as usize; 32]; N_COUNTERS]) },
            pipes: Vec::with_capacity(len),
            clean: clean,
            gc: Collector::new(),
        };
        for _ in 0..len {
            res.pipes.push(B::new())
        }
        fence(Ordering::Acquire);
        res
    }

    // Creates a new `BagPipeState` with a small number of pipes.
    pub fn new(clean: Clean) -> Self {
        Self::new_size(num_cpus::get() * 2, clean)
    }

    // Attempts to push `it` down a pipe, following a schedule specified
    // by offset, allowing for at most `max_failures` failures.
    pub fn try_push_internal(
        &self,
        guard: &Guard,
        it: B::Item,
        offset: usize,
        stride: usize,
        max_failures: usize,
        succeed_final: bool,
    ) -> Result<bool, B::Item> {
        let mut ix = offset;
        let mut remaining = max_failures;
        let len = self.pipes.len();
        debug_assert!(len.is_power_of_two());
        let mut cur_item = it;
        while remaining > 0 {
            ix &= len - 1;
            unsafe {
                if succeed_final && remaining == 1 {
                    self.pipes.get_unchecked(ix).push(guard, cur_item);
                    // indicates whether the final cell was used
                    return Ok(false);
                } else {
                    match self.pipes.get_unchecked(ix).try_push(guard, cur_item) {
                        Ok(()) => return Ok(true),
                        Err(item) => cur_item = item,
                    }
                }
            }
            ix += stride;
            remaining -= 1;
        }
        Err(cur_item)
    }

    pub fn try_pop_internal(
        &self,
        guard: &Guard,
        offset: usize,
        stride: usize,
        max_failures: usize,
    ) -> PopResult<B::Item> {
        let mut ix = offset;
        let mut remaining = max_failures;
        let mut empties = 0;
        let len = self.pipes.len();
        debug_assert!(len.is_power_of_two());
        debug_assert!(max_failures > 0);
        #[cfg(debug_assertions)]
        let mut seen = Vec::new();
        loop {
            ix &= len - 1;
            unsafe {
                match self.pipes.get_unchecked(ix).try_pop(guard) {
                    Ok(it) => return Ok(it),
                    Err(PopStatus::Empty) => {
                        #[cfg(debug_assertions)] seen.push(ix);
                        empties += 1
                    }
                    Err(PopStatus::TransientFailure) => empties = 0,
                }
            };
            ix += stride;
            remaining -= 1;
            if remaining == 0 {
                return Err(PopStatus::TransientFailure);
            }
            // This is not linearizable, but I believe it is
            // sequentially consistent.
            if empties == len {
                #[cfg(debug_assertions)]
                {
                    seen.sort();
                    seen.dedup();
                    let expected: Vec<usize> = (0..len).collect();
                    assert_eq!(
                        seen,
                        expected,
                        "got {:?} but expected {:?}, with offset={} and stride={}",
                        seen,
                        expected,
                        offset,
                        stride
                    );
                }
                return Err(PopStatus::Empty);
            }
        }
    }
}
