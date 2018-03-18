// Copyright 2017-2018 the authors. See the 'Copyright and license' section of the
// README.md file at the top-level directory of this repository.
//
// Licensed under the Apache License, Version 2.0 (the LICENSE-APACHE file) or
// the MIT license (the LICENSE-MIT file) at your option. This file may not be
// copied, modified, or distributed except according to those terms.

//! Implementation of two non-blocking queues.
//!
//! - `GeneralYC`, a fast, best-effort queue that will fail
//!   occasionally. See documentation of `YangCrummeyQueue` for more
//!   information on the algorithm.
//! - `FAAArrayQueue`, a similar design reimplemented in rust. This is
//!   lock-free and always succeeds (i.e. it loops). See documentation on
//!   `FAAQueueLowLevel` for more information on the algorithm
//!
//! Both of these data-structures are generated from lower-level
//! implementations that operate only on word-sized types for efficient
//! atomic operations.
//!
//! The `generalize` macro takes these low-level `SharedWeakBag`s only
//! operating on word-sized `Node`s and lifts them to `SharedWeakBag`s
//! operating on arbitrary types by boxing them and passing the
//! underlying bag a raw pointer.
//!
//! We expose the low-level data-structures (`FAAQueueLowLevel`
//! and `YangCrummeyQueue` for `FAAArrayQueue` and `GeneralYC`,
//! respectively) both for benchmarking purposes and to facilitate
//! documentation; it does not seem possible to generate custom doc
//! comments for those data-structures.

use super::crossbeam_epoch::{Shared, Atomic, Owned, Guard};
use super::crossbeam::mem::CachePadded;
use bag::{PopResult, PopStatus, SharedWeakBag, Revocable, RevocableWeakBag};
use std::sync::atomic::{AtomicUsize, Ordering, fence};
use std::ptr;
use std::mem;
use std::marker::PhantomData;
use std::fmt::{Debug, Formatter};

/// The number of elements in a given `Segment` link.
const SEG_SIZE: usize = 1 << SEG_SHIFT;
/// The base-2 log of `SEG_SIZE`. In debug builds having size this large
/// causes a seg fault.
///
/// More thought should be put into how we set this value. For best
/// performance, a high value for `SEG_SHIFT` is desirable. Currently
/// this is 15 which amounts to 16MiB for an empty `BagPipe` on a
/// 64-pipe setup. Quite solid performance can be had for `SEG_SHIFT`=10
/// (making the equivalent bagpiep about 524K)
///
/// TODO(ezrosent): There is a benefit to having this being a
/// statically-sized array, as opposed to a vector.  However I believe
/// there is a small enough set of reasonable sizes that this can
/// be goverend by feature flags or, in the worst case, a macro for
/// generating custom queues for a pre-set menu of sizes.
#[cfg(debug_assertions)]
const SEG_SHIFT: usize = 12;
#[cfg(all(not(debug_assertions), not(feature = "huge_segments")))]
const SEG_SHIFT: usize = 15;
#[cfg(all(feature = "huge_segments", not(debug_assertions)))]
const SEG_SHIFT: usize = 16;

/// Segment size for `FAAQueue` family of queues.
///
/// TODO(ezrosent) We should just have each queue in its own module with
/// its own segment size
const SMALL_SEG_SIZE: usize = 1 << SMALL_SEG_SHIFT;
const SMALL_SEG_SHIFT: usize = 8;
mod node_inner {
    //! Contains definition and implementations for the `Node` trait.
    use super::*;

    /// A word-sized type that can behave like a `usize`.
    ///
    /// "Safe" implementations are given for integral and
    /// raw pointer types.
    pub trait Node
    where
        Self: Sized + Copy,
    {
        fn zeros() -> [Self; SEG_SIZE];
        fn zeros_small() -> [Self; SMALL_SEG_SIZE];
        fn as_usize(self) -> usize;
        fn from_usize(usize) -> Self;
    }

    macro_rules! ptr_impl {
        ($ptr:ty, $ty: tt) => {
            impl<$ty: Sized> Node for $ptr {
                fn zeros() -> [Self; SEG_SIZE] {
                    debug_assert_eq!(ptr::null() as *const $ty as usize, 0);
                    unsafe {
                        mem::transmute([0 as usize; SEG_SIZE])
                    }
                }
                fn zeros_small() -> [Self; SMALL_SEG_SIZE] {
                    debug_assert_eq!(ptr::null() as *const $ty as usize, 0);
                    unsafe {
                        mem::transmute([0 as usize; SMALL_SEG_SIZE])
                    }
                }
                fn as_usize(self) -> usize {
                    let res = self as usize;
                    debug_assert_ne!(res, SENTINEL);
                    debug_assert_ne!(res, 0);
                    res
                }

                fn from_usize(u: usize) -> Self {
                    u as Self
                }
            }
        };
    }

    ptr_impl!(*mut T, T);
    ptr_impl!(*const T, T);


    macro_rules! impl_node {
        // for numeric types, we add and subtract one when converting to and from this
        // representation. This allows callers to safely push 0 and not run afoul of any
        // data-structure invariants. If callers push all 1s, they get overflow, but this is
        // already documented as UB for the `YangCrummeyQueue` anyway.
        ($typ:tt) => {
            impl Node for $typ {
                fn zeros() -> [Self; SEG_SIZE] {
                    // this enforces that $typ is word-sized.
                    [0; SEG_SIZE]
                }
                fn zeros_small() -> [Self; SMALL_SEG_SIZE] {
                    // this enforces that $typ is word-sized.
                    [0; SMALL_SEG_SIZE]
                }
                fn as_usize(self) -> usize {
                    let res = (self+1) as usize;
                    debug_assert_ne!(res, 0);
                    debug_assert_ne!(res, SENTINEL);
                    res
                }

                fn from_usize(u: usize) -> Self {
                    debug_assert_ne!(u, 0);
                    debug_assert_ne!(u, SENTINEL);
                    (u-1) as Self
                }
            }
        };
    }

    impl_node!(isize);
    impl_node!(usize);

    #[cfg(target_pointer_width = "32")]
    impl_node!(i32);
    #[cfg(target_pointer_width = "32")]
    impl_node!(u32);
    #[cfg(target_pointer_width = "64")]
    impl_node!(i64);
    #[cfg(target_pointer_width = "64")]
    impl_node!(u64);
}

use self::node_inner::Node;

#[inline]
fn assert_node<T: Node>(_t: &T) {}

macro_rules! node_to_atomic_ref {
    ($nd:expr) => {
        {
            let _nd = $nd;
            assert_node(_nd);
            #[allow(unused_unsafe)]
            unsafe { mem::transmute::<_, &AtomicUsize>(_nd) }
        }
    };
}

/// A sentinel value used in `YangCrummeyQueue`.
const SENTINEL: usize = !0;

/// A scalable queue with fairly weak guarantees.
///
/// The `YangCrummeyQueue` is a `SharedWeakBag` based on the
/// obstruction-free queue described in Yang and Crummey's ["A Wait-Free
/// Queue as Fast as Fetch and Add"][1] as well as Morisson and Afek's
/// ["Fast Concurrent Queues For X86 Processors"][2]. In the common case
/// a single push or pop is a fetch-add, a load, some bit-shifts and an
/// uncontended CAS. If called in a loop, `try_push` and `try_pop` are
/// both obstruction-free.
///
/// Note that pushing values with the same byte-level representation as
/// `SENTINEL` (all 1s) or `SENTINEL-1` is undefined; an assertion will
/// catch this in a build with `debug_assert` enabled.
///
/// ## Why Name it after Yang and Crummey?
///
/// While Morrison and Afek describe a similar informal obstruction-free
/// "infinite-array" queue as Yang and Crummey at an earlier point in
/// time, the actual realization of the infinite-array queue in these
/// two works is very different. It is the later design of a linked list
/// of `Segment`s, each with their own `id` that we take as inspiration
/// here, hence the naming decision.
///
/// [1]: http://chaoran.me/assets/pdf/wfq-ppopp16.pdf
/// [2]: http://www.cs.tau.ac.il/~mad/publications/ppopp2013-x86queues.pdf
pub struct YangCrummeyQueue<T: Node> {
    head_index: CachePadded<AtomicUsize>,
    tail_index: CachePadded<AtomicUsize>,
    head_data: CachePadded<Atomic<Segment<T>>>,
    tail_data: CachePadded<Atomic<Segment<T>>>,
}

impl<T: Node> Drop for YangCrummeyQueue<T> {
    fn drop(&mut self) {
        // This method will do very bad things if it ever gets called
        // with parts of the queue still reachable. Since we have exclusive
        // access (&mut parameter), however, we're guaranteed that can't
        // happen. Thus, use a dummy Guard.
        let guard = unsafe { ::crossbeam_epoch::unprotected() };

        let head = unsafe { self.head_data.load(Ordering::Relaxed, &guard).deref() };
        let tail = unsafe { self.tail_data.load(Ordering::Relaxed, &guard).deref() };
        let mut cur_node = if head.id.load(Ordering::Relaxed) < tail.id.load(Ordering::Relaxed) {
            head
        } else {
            tail
        };
        loop {
            unsafe {
                guard.defer(move || mem::drop(cur_node));
            }
            let shared = cur_node.next.load(Ordering::Relaxed, &guard);
            if shared.is_null() {
                break;
            }
            cur_node = unsafe { shared.deref() };
        }
    }
}

unsafe impl<T: Node> Send for YangCrummeyQueue<T> {}
unsafe impl<T: Node> Sync for YangCrummeyQueue<T> {}

impl<T: Node> YangCrummeyQueue<T> {
    /// Attempt to reclaim unused segments.
    ///
    /// `try_cleanup` is called when increment_and_get_usize notices
    /// that it traversed an additional pointer when looking up a
    /// value, it attempts to advance the `seg` pointer and reclaim the
    /// corresponding memory if necessary.
    fn try_cleanup<'a>(&self, seg: &Atomic<Segment<T>>, data: Shared<'a, Segment<T>>, g: &Guard) {
        // TODO(ezr) relaxed semantics may be sufficient here.
        let next = unsafe { data.deref().next.load(Ordering::Acquire, g) };
        // if next is null, then it means that something very strange
        // has happened and it is best to walk away. Recall that
        // try_cleanup is only called if some thread actually followed
        // seg's next pointer. Even for a very long stall in execution,
        // some segment must be there. Given the use of relaxed ordering
        // in push and pop I am not sure that this can be ruled out.
        if !next.is_null()
            // next we try and CAS data to its next pointer. If this
            // fails it means someone else succeeded and we can just
            // return.
            && seg.compare_and_set(data, next, Ordering::Release, g).is_ok()
            // finally, we increment the counter of times this value
            // has advanced. data.ctr is a sort of "inverse ref count";
            // it avoids a double-free with head_data and tail_data
            // advancing past the same segment. Whoever advances the
            // pointer last is obligated to reclaim the memory.
            && unsafe { data.deref().ctr.fetch_add(1, Ordering::AcqRel) == 1 }
        {
            // we are the second pointer to advance past this segment,
            // so we reclaim the memory.
            unsafe { g.defer(move || mem::drop(data)) }
        }
    }


    // Helper method to increment `ind` and find the corresponding index
    // and cell starting in `seg`, advancing `seg` if need be.
    fn increment_and_get_usize<'a>(
        &self,
        ind: &AtomicUsize,
        seg: &Atomic<Segment<T>>,
        g: &'a Guard,
    ) -> (usize, &'a AtomicUsize) {
        let data = seg.load(Ordering::Relaxed, g);
        assert!(!data.is_null());
        let cur_id = unsafe { data.deref().id.load(Ordering::Relaxed) };
        // the load of data must happen before the fetch-add of ix,
        // otherwise we are in danger of the pointer being advanced
        // between ix being incremented data being loaded.
        fence(Ordering::Acquire);
        let ix = ind.fetch_add(1, Ordering::Relaxed);
        let (node_ref, try_clean) = unsafe { data.deref().find_cell(ix, g) };
        let res = node_to_atomic_ref!(node_ref);
        // only try to clean up if ix corresponds to the first index
        // past cur_id.
        //
        // TODO(ezrosent): the second check should obviate the need for
        // try_clean in the first place.
        if try_clean && ((ix >> SEG_SHIFT) == (cur_id + 1)) {
            self.try_cleanup(seg, data, g)
        }
        (ix, res)
    }

    #[cfg(feature = "check_empty_yq")]
    fn is_empty(&self) -> bool {
        let h = self.head_index.load(Ordering::Relaxed);
        fence(Ordering::Acquire);
        let t = self.tail_index.load(Ordering::Relaxed);
        h >= t
    }
}

impl<T: Node> SharedWeakBag for YangCrummeyQueue<T> {
    type Item = T;

    fn new() -> Self {
        // Since we're only accessing data created in this function,
        // and we haven't spawned any new threads, we don't actually
        // need GC yet. Thus, just use a dummy guard.
        let guard = unsafe { ::crossbeam_epoch::unprotected() };

        // Initialize head and tail pointers to point to the same location.
        let res = YangCrummeyQueue {
            head_index: CachePadded::new(AtomicUsize::new(0)),
            tail_index: CachePadded::new(AtomicUsize::new(0)),
            head_data: CachePadded::new(Atomic::new(Segment::new(0))),
            tail_data: CachePadded::new(Atomic::null()),
        };
        res.tail_data.store(
            res.head_data.load(Ordering::SeqCst, &guard),
            Ordering::Release,
        );
        res
    }

    fn try_push(&self, guard: &Guard, t: T) -> Result<(), T> {
        let (_, cell) = self.increment_and_get_usize(&self.tail_index, &self.tail_data, guard);
        // XXX: can this be an exchange instead of a CAS?
        let res = cell.swap(t.as_usize(), Ordering::Relaxed);
        debug_assert!(res == 0 || res == SENTINEL);
        if res == 0 { Ok(()) } else { Err(t) }
    }

    fn try_pop(&self, guard: &Guard) -> PopResult<T> {
        #[cfg(feature = "check_empty_yq")]
        {
            if self.is_empty() {
                return Err(PopStatus::Empty);
            }
        }
        let (ix, cell) = self.increment_and_get_usize(&self.head_index, &self.head_data, guard);
        let res = cell.swap(SENTINEL, Ordering::Relaxed);
        debug_assert_ne!(res, SENTINEL);
        if res == 0 {
            // The CAS succeeded, meaning we were the first one to this
            // cell. Time to see if there was a race, or the queue was
            // just empty.

            // This fence ensures that the load of tail_index is ordered
            // after the load of head_index in increment_and_get_usize.
            fence(Ordering::Acquire);
            let tail = self.tail_index.load(Ordering::Relaxed);
            // if ix is greater than tail then there are no nodes that
            // we could have gotten. Otherwise, it is possible that we
            // missed something.
            if ix > tail {
                Err(PopStatus::Empty)
            } else {
                Err(PopStatus::TransientFailure)
            }
        } else {
            Ok(Node::from_usize(res))
        }
    }

    fn debug(&self) {
        println!("{:?}", self)
    }
}

/// A lazily initialized concurrent linked list.
///
/// A `Segment` forms a core building block of a few data-structures. It
/// is a lazily-initialized "infinite array" implemented as a linked
/// list of arrays.
struct Segment<T: Node> {
    id: CachePadded<AtomicUsize>,
    ctr: CachePadded<AtomicUsize>,
    data: [T; SEG_SIZE],
    next: CachePadded<Atomic<Segment<T>>>,
}

impl<T: Node> Segment<T> {
    pub fn new(id: usize) -> Self {
        Segment {
            id: CachePadded::new(AtomicUsize::new(id)),
            ctr: CachePadded::new(AtomicUsize::new(0)),
            data: T::zeros(),
            next: CachePadded::new(Atomic::null()),
        }
    }

    pub fn find_cell<'a, 'b: 'a>(
        &'b self,
        ix: usize,
        guard: &'a Guard,
    ) -> (&'a T, bool /* TODO(ezrosent): remove this */) {
        // First, figure out which segment we want, and where in that
        // segment the location is.
        #[cfg(feature="staggered_indexes")]
        let (seg_id, seg_ix) = (ix >> SEG_SHIFT, (ix * 7) & (SEG_SIZE - 1));
        #[cfg(not(feature="staggered_indexes"))]
        let (seg_id, seg_ix) = (ix >> SEG_SHIFT, ix & (SEG_SIZE - 1));
        let mut cur_seg = self;
        let mut not_first = false;
        // post-condition: cur_seg.id == seg_id.
        //
        // Note that we use Relaxed ordering everywhere. This is okay
        // because the only coordination needed among threads here is
        // to ensure new segments are added. Because the correct ID is
        // computed independently of all other threads (and each segment
        // id is fixed from its inception), all we need to guarantee
        // this is the atomicity of compare and swap. No additional
        // ordering is required.
        loop {
            let cur_id = cur_seg.id.load(Ordering::Relaxed);
            debug_assert!(cur_id <= seg_id);
            if cur_id == seg_id {
                break;
            }
            not_first = true;
            // our target segment is further in the list.
            let next = cur_seg.next.load(Ordering::Relaxed, guard);
            if next.is_null() {
                // There is no next segment, attempt to create it. If
                // this compare-and-swap fails it means that someone
                // else suceeded, so we just retry.
                let new_seg = Owned::new(Segment::new(cur_id + 1));
                // TODO(ezr) don't waste the allocation?
                if let Ok(shared) = cur_seg.next.compare_and_set(
                    Shared::null(),
                    new_seg,
                    Ordering::Relaxed,
                    guard,
                )
                {
                    cur_seg = unsafe { shared.deref() };
                }
            } else {
                cur_seg = unsafe { next.deref() };
            }
        }
        (unsafe { cur_seg.data.get_unchecked(seg_ix) }, not_first)
    }
}

/// Another fetch-add based concurrent queue.
///
/// This queue is based on the `FAAArrayQueue` of Pedro Ramalhete
/// over at [Concurrency Freaks][1].  The implementation here is a
/// direct adaptation of the C++ code for the algorithm, the only
/// difference being that we perform the same "decomposition" of the
/// algorithm into its inner loop so that the `BagPipe` can take better
/// advantage of contention information. Still, the default `push` and
/// `pop` implementations are lock-free for the same reason that the
/// `FAAArrayQueue` is lock-free. This means it has stronger progress
/// guarantees than the `YangCrummeyQueue`.
///
/// ## This or `YangCrummeyQueue`
///
/// It depends. The `FAAArrayQueue` generally performs better on its
/// own, but it lags behind the `YangCrummeyQueue` at 32 cores for some
/// workloads in a `BagPipe` configuration. If performance is a chief
/// concern, it is worth benchmarking both.
///
/// [1]: http://concurrencyfreaks.blogspot.com/2016/11/faaarrayqueue-mpmc-lock-free-queue-part.html
pub struct FAAQueueLowLevel<T: Node, F = ()>
where
    F: RevokeFunc<T> + 'static,
{
    head: CachePadded<Atomic<FAANode<T>>>,
    tail: CachePadded<Atomic<FAANode<T>>>,
    _marker: PhantomData<F>,
}

pub type RevocableFAAQueue<T> = FAAQueueLowLevel<T, Revoker<T>>;

pub trait RevokeFunc<T> {
    fn store(&T, usize);
}

pub struct Revoker<T>(PhantomData<T>);

impl<T: Revocable> RevokeFunc<T> for Revoker<T> {
    // TODO(ezrosent) measure this with
    // #[inline(always)]
    fn store(item: &T, value: usize) {
        item.handle().store(value, Ordering::Relaxed)
    }
}

impl<T> RevokeFunc<T> for () {
    #[inline(always)]
    fn store(_t: &T, _u: usize) {}
}

impl<T: Node, F: RevokeFunc<T> + 'static> Drop for FAAQueueLowLevel<T, F> {
    fn drop(&mut self) {
        // TODO(joshlf): Can this just be a dummy guard?
        let guard = ::crossbeam_epoch::pin();
        let mut cur_node = &self.head;
        let mut n = cur_node.load(Ordering::Relaxed, &guard);
        while !n.is_null() {
            unsafe {
                guard.defer(move || mem::drop(n));
            }
            cur_node = unsafe { &n.deref().next };
            n = cur_node.load(Ordering::Relaxed, &guard);
        }
    }
}

impl<T: Node + Revocable + 'static> RevocableWeakBag for FAAQueueLowLevel<T, Revoker<T>> {
    unsafe fn revoke(it: &Self::Item) -> bool {
        let from = it.as_usize();
        let item_ptr = (it.handle().load(Ordering::Relaxed) as *mut AtomicUsize).as_ref();
        match item_ptr {
            None => false,
            Some(item_ref) => {
                let res = item_ref.compare_and_swap(from, SENTINEL, Ordering::Relaxed);
                res == from
            }
        }
    }
}

impl<T: Node, F: RevokeFunc<T> + 'static> SharedWeakBag for FAAQueueLowLevel<T, F> {
    type Item = T;

    fn new() -> Self {
        // Since we're only accessing data created in this function,
        // and we haven't spawned any new threads, we don't actually
        // need GC yet. Thus, just use a dummy guard.
        let guard = unsafe { ::crossbeam_epoch::unprotected() };

        let res = FAAQueueLowLevel {
            head: CachePadded::new(Atomic::new(FAANode::new_empty())),
            tail: CachePadded::new(Atomic::null()),
            _marker: PhantomData,
        };
        res.tail.store(
            res.head.load(Ordering::Relaxed, &guard),
            Ordering::Relaxed,
        );
        res
    }

    fn try_push(&self, guard: &Guard, item: T) -> Result<(), T> {
        let it = item.as_usize();
        loop {
            let tail = self.tail.load(Ordering::Relaxed, guard);
            assert!(!tail.is_null());
            let ix = unsafe { tail.deref().tail_index.fetch_add(1, Ordering::Relaxed) };
            if ix >= SMALL_SEG_SIZE {
                // We need to enqueue in a cell further in the linked list.
                //
                // First we check if the tail has changed; if it has we reload it.
                if !self.is_tail(tail, guard) {
                    continue;
                }
                let next = unsafe { tail.deref().next.load(Ordering::Relaxed, guard) };
                if next.is_null() {
                    // There is nothing in the tail value, so we attempt to allocate a new node
                    // beginning with our item.
                    let new_node = Owned::new(FAANode::new(Node::from_usize(it)));
                    if let Ok(new_node) = unsafe { tail.deref().next.compare_and_set(
                        Shared::null(),
                        new_node,
                        Ordering::Relaxed,
                        guard,
                    ) } {
                        // We succeeded. We try to CAS tail to our new segment but either way
                        // we can return success.
                        let _ = self.tail.compare_and_set(
                            tail,
                            new_node,
                            Ordering::Relaxed,
                            guard,
                        );
                        return Ok(());
                    }
                    // We failed, but that means another node is there, so we retry.
                } else {
                    // There is an item after the current tail, we try and assign it to the
                    // value of the tail. Regardless of the success of the CAS, there will be
                    // something new in the value of the tail, so we reload.
                    let _ = self.tail.compare_and_set(
                        tail,
                        next,
                        Ordering::Relaxed,
                        guard,
                    );
                    continue;
                }
                continue;
            }
            // ix is within range. We try and swap in our value.
            let cell_ref = unsafe { tail.deref().get(ix) };
            // store revocation information if available.
            F::store(&item, cell_ref as *const _ as *mut T as usize);
            let res = cell_ref.compare_and_swap(0, it, Ordering::Relaxed);
            return if res == 0 {
                Ok(())
            } else {
                #[cfg(debug_assertion)]
                {
                    // re-set this to zero on debug builds, to clarify the state transition.
                    F::store(&item, 0);
                }
                Err(item)
            };
        }
    }

    fn try_pop(&self, guard: &Guard) -> PopResult<T> {
        loop {
            let head = self.head.load(Ordering::Relaxed, guard);
            assert!(!head.is_null());
            // First we check if the queue is empty. To do this we load the head and then the tail.
            // This is because both values can only increase, but if tail_ix is less than what
            // head_ix was in the past, the queue is still empty regardless (because no concurrent
            // pop could increment head).
            let head_ix = unsafe { head.deref().head_index.load(Ordering::Relaxed) };
            fence(Ordering::Acquire);
            let tail_ix = unsafe { head.deref().tail_index.load(Ordering::Relaxed) };
            let tail_ptr = unsafe { head.deref().next.load(Ordering::Relaxed, guard) };
            if head_ix >= tail_ix && tail_ptr.is_null() {
                return Err(PopStatus::Empty);
            }
            // Now we acquire a location in the cell.
            let my_ix = unsafe { head.deref().head_index.fetch_add(1, Ordering::Relaxed) };
            if my_ix >= SMALL_SEG_SIZE {
                // We are past the end, time to see if there is anything forward in the list.
                let next = unsafe { head.deref().next.load(Ordering::Relaxed, guard) };
                if next.is_null() {
                    return Err(PopStatus::Empty);
                } else {
                    // There is something, we try and CAS the head to it and retry. Because
                    // this segment is now exhausted, we mark it as unlinked. Why is this safe?
                    // because we already confirmed that the queue is non-empty. This means
                    // that either all active dequeue threads will either look to the next node
                    // already, or have an active guard.
                    //
                    // TODO(ezrosent): confirm this reasoning.
                    if self.head.compare_and_set(
                        head,
                        next,
                        Ordering::Relaxed,
                        guard,
                    ).is_ok()
                    {
                        unsafe { guard.defer(move || mem::drop(head)) };
                    }
                    continue;
                }
            }
            // Now we try and swap sentinel into our chosen location.
            let res = unsafe { head.deref().get(my_ix).swap(SENTINEL, Ordering::Relaxed) };
            return if res == 0 || res == SENTINEL {
                // revocations can put SENTINEL in any cell
                Err(PopStatus::TransientFailure)
            } else {
                let res_t = Node::from_usize(res);
                // erase the stamp; N.B: this may not be necessary for the allocator material, but
                // it is very unlikely to be a bottleneck.
                F::store(&res_t, 0);
                Ok(res_t)
            };
        }
    }
}

impl<T: Node, F: RevokeFunc<T> + 'static> FAAQueueLowLevel<T, F> {
    fn is_tail<'a>(&self, ptr: Shared<'a, FAANode<T>>, guard: &'a Guard) -> bool {
        let raw_ptr = self.tail.load(Ordering::Relaxed, guard);
        ptr.as_raw() == raw_ptr.as_raw()
    }
}

struct FAANode<T> {
    head_index: CachePadded<AtomicUsize>,
    tail_index: CachePadded<AtomicUsize>,
    data: [T; SMALL_SEG_SIZE],
    next: CachePadded<Atomic<FAANode<T>>>,
}

impl<T: Node> FAANode<T> {
    fn new_empty() -> Self {
        FAANode {
            head_index: CachePadded::new(AtomicUsize::new(0)),
            tail_index: CachePadded::new(AtomicUsize::new(0)),
            data: T::zeros_small(),
            next: CachePadded::new(Atomic::null()),
        }
    }
    fn new(item: T) -> Self {
        let res = Self::new_empty();
        res.tail_index.store(1, Ordering::Relaxed);
        node_to_atomic_ref!(unsafe { res.data.get_unchecked(0) })
            .store(item.as_usize(), Ordering::Relaxed);
        res
    }

    unsafe fn get(&self, ix: usize) -> &AtomicUsize {
        node_to_atomic_ref!(self.data.get_unchecked(ix))
    }
}

macro_rules! generalize {
    ($curty:ident, $newty:ident) => {
        /// A macro-generated general data-structure supporting arbitrary `Copy` types.
        pub struct $newty<T>($curty<*mut T>);
        unsafe impl<T> Send for $newty<T> {}
        unsafe impl<T> Sync for $newty<T> {}
        impl<T: Sized> SharedWeakBag for $newty<T> {
            type Item = T;

            fn new() -> Self {
                $newty($curty::new())
            }

            fn try_push(&self, guard: &Guard, t: T) -> Result<(), T> {
                let raw_t: *mut T = Box::into_raw(Box::new(t));
                match self.0.try_push(guard, raw_t) {
                    Ok(()) => Ok(()),
                    Err(it) => unsafe { Err(*Box::from_raw(it)) },
                }
            }

            fn try_pop(&self, guard: &Guard) -> PopResult<T> {
                match self.0.try_pop(guard) {
                    Ok(raw_ptr) => unsafe { Ok(*Box::from_raw(raw_ptr)) },
                    Err(status) => Err(status),
                }
            }
        }

        impl<T> Drop for $newty<T> {
            fn drop(&mut self) {
                // Dummy handle. Since drop takes a mutable reference, we know that
                // there are no concurrent accesses, so the GC is pointless here,
                // so creating a dummy one is fine.
                let guard = unsafe { ::crossbeam_epoch::unprotected() };
                loop {
                    match self.0.try_pop(&guard) {
                        Err(PopStatus::TransientFailure) | Ok(_) => continue,
                        Err(PopStatus::Empty) => return,
                    };
                }
            }
        }

    };
}

/// A version of the `YangCrummeyQueue` supporting arbitrary types.
generalize!(YangCrummeyQueue, GeneralYC);
/// A version of the `FAAQueueLowLevel` supporting arbitrary types.
generalize!(FAAQueueLowLevel, FAAArrayQueue);

#[cfg(test)]
mod tests {
    use super::*;
    use std::thread;
    use std::sync::{Arc, Barrier};
    use std::sync::mpsc::channel;
    use crossbeam_epoch::default_handle;

    fn single_threaded_enqueue_dequeue<W: SharedWeakBag<Item = usize>>(size: usize) {
        let yq = W::new();
        for i in 0..size {
            assert!(yq.try_push(&default_handle().pin(), i).is_ok())
        }
        for i in 0..size {
            if let Ok(item) = yq.try_pop(&default_handle().pin()) {
                assert_eq!(item, i);
            } else {
                panic!("failed to pop non-empty queue (try {})", i)
            }
        }
    }

    #[test]
    fn single_threaded_small() {
        single_threaded_enqueue_dequeue::<YangCrummeyQueue<usize>>(64)
    }

    #[test]
    fn single_threaded_large() {
        single_threaded_enqueue_dequeue::<YangCrummeyQueue<usize>>(SEG_SIZE * 8)
    }

    #[test]
    fn single_threaded_small_faaq() {
        single_threaded_enqueue_dequeue::<FAAQueueLowLevel<usize>>(64)
    }

    #[test]
    fn single_threaded_large_faaq() {
        single_threaded_enqueue_dequeue::<FAAQueueLowLevel<usize>>(SEG_SIZE * 8)
    }

    #[test]
    fn single_threaded_small_faaqg() {
        single_threaded_enqueue_dequeue::<FAAArrayQueue<usize>>(64)
    }

    #[test]
    fn single_threaded_large_faaqg() {
        single_threaded_enqueue_dequeue::<FAAArrayQueue<usize>>(SEG_SIZE * 8)
    }

    #[test]
    fn single_threaded_small_general() {
        single_threaded_enqueue_dequeue::<GeneralYC<usize>>(64)
    }

    #[test]
    fn single_threaded_large_general() {
        single_threaded_enqueue_dequeue::<GeneralYC<usize>>(SEG_SIZE * 8)
    }

    fn two_threads_enqueue_dequeue<W>(size: usize)
    where
        W: SharedWeakBag<Item = usize> + Send + Sync + 'static,
    {
        let bar = Arc::new(Barrier::new(3));
        let yq = Arc::new(W::new());
        let (yq1, bar1) = (yq.clone(), bar.clone());
        let j1 = thread::spawn(move || {
            bar1.wait();
            for i in 0..size {
                while yq1.try_push(&default_handle().pin(), i).is_err() {}
            }
        });
        let (yq2, bar2) = (yq.clone(), bar.clone());
        let j2 = thread::spawn(move || {
            bar2.wait();
            for i in 0..size {
                loop {
                    if let Ok(item) = yq2.try_pop(&default_handle().pin()) {
                        assert_eq!(item, i);
                        break;
                    }
                }
            }
        });
        bar.wait();
        j1.join().expect("sending thread should succeed");
        j2.join().expect("receiving thread should succeed");
    }

    #[test]
    fn two_threads_enqueue_dequeue_small() {
        two_threads_enqueue_dequeue::<YangCrummeyQueue<usize>>(64)
    }

    #[test]
    fn two_threads_enqueue_dequeue_large() {
        two_threads_enqueue_dequeue::<YangCrummeyQueue<usize>>(SEG_SIZE * 8)
    }

    #[test]
    fn two_threads_enqueue_dequeue_small_general() {
        two_threads_enqueue_dequeue::<GeneralYC<usize>>(64)
    }

    #[test]
    fn two_threads_enqueue_dequeue_large_general() {
        two_threads_enqueue_dequeue::<GeneralYC<usize>>(SEG_SIZE * 8)
    }

    #[test]
    fn two_threads_enqueue_dequeue_small_faaq() {
        two_threads_enqueue_dequeue::<FAAQueueLowLevel<usize>>(64)
    }

    #[test]
    fn two_threads_enqueue_dequeue_large_faaq() {
        two_threads_enqueue_dequeue::<FAAQueueLowLevel<usize>>(SEG_SIZE * 8)
    }

    #[test]
    fn two_threads_enqueue_dequeue_small_faaqg() {
        two_threads_enqueue_dequeue::<FAAArrayQueue<usize>>(64)
    }

    #[test]
    fn two_threads_enqueue_dequeue_large_faaqg() {
        two_threads_enqueue_dequeue::<FAAArrayQueue<usize>>(SEG_SIZE * 8)
    }

    fn many_threads_no_lost_nodes<W>(size: usize, nthreads: usize)
    where
        W: SharedWeakBag<Item = usize> + Send + Sync + 'static,
    {
        let bar = Arc::new(Barrier::new(nthreads * 2 + 1));
        let yq = Arc::new(W::new());
        let (sender, recv) = channel();
        let mut threads = Vec::new();
        for tnum in 0..nthreads {
            let (yqt, bart) = (yq.clone(), bar.clone());
            threads.push(thread::spawn(move || {
                bart.wait();
                for i in 0..size {
                    while yqt.try_push(&default_handle().pin(), tnum * size + i).is_err() {}
                }
            }));
        }


        for _ in 0..nthreads {
            let (yqt, bart) = (yq.clone(), bar.clone());
            let mychan = sender.clone();
            threads.push(thread::spawn(move || {
                // For debugging issues like the empty check being faulty.
                bart.wait();
                loop {
                    match yqt.try_pop(&default_handle().pin()) {
                        Err(PopStatus::Empty) => break,
                        Err(PopStatus::TransientFailure) => {}
                        Ok(item) => mychan.send(item).expect("channel send should succeed"),
                    }
                }
            }));
        }
        bar.wait();
        for t in threads.into_iter() {
            t.join().expect("all threads should exit cleanly")
        }

        // clean up any stragglers
        loop {
            match yq.try_pop(&default_handle().pin()) {
                Err(PopStatus::Empty) => break,
                Err(PopStatus::TransientFailure) => {}
                Ok(item) => sender.send(item).expect("channel send should succeed"),
            }
        }

        mem::drop(sender);
        let mut results = Vec::new();
        while let Ok(item) = recv.recv() {
            results.push(item)
        }
        results.sort();
        let expected: Vec<usize> = (0..(nthreads * size)).collect();
        if results != expected {
            println!("expected {} numbers got {}", expected.len(), results.len());
            let mut cur_expected = 0;
            for it in results.into_iter() {
                while it > cur_expected {
                    println!("[{}]: missing {}", it, cur_expected);
                    cur_expected += 1;
                }
                cur_expected += 1;
            }
            panic!("did not get all values")
        }
    }

    #[test]
    fn many_threads_small() {
        many_threads_no_lost_nodes::<YangCrummeyQueue<usize>>(128, 4)
    }

    #[test]
    fn many_threads_large() {
        many_threads_no_lost_nodes::<YangCrummeyQueue<usize>>(1024, 64)
    }

    #[test]
    fn many_threads_small_general() {
        many_threads_no_lost_nodes::<GeneralYC<usize>>(128, 4)
    }

    #[test]
    fn many_threads_large_general() {
        many_threads_no_lost_nodes::<GeneralYC<usize>>(1024, 64)
    }

    #[test]
    fn many_threads_small_faaq() {
        many_threads_no_lost_nodes::<FAAQueueLowLevel<usize>>(128, 4)
    }

    #[test]
    fn many_threads_large_faaq() {
        many_threads_no_lost_nodes::<FAAQueueLowLevel<usize>>(1024, 64)
    }

    #[test]
    fn many_threads_small_faaqg() {
        many_threads_no_lost_nodes::<FAAArrayQueue<usize>>(128, 4)
    }

    #[test]
    fn many_threads_large_faaqg() {
        many_threads_no_lost_nodes::<FAAArrayQueue<usize>>(1024, 64)
    }
}

// Miscellany.

impl<T: Node> Debug for GeneralYC<T> {
    fn fmt(&self, f: &mut Formatter) -> ::std::fmt::Result {
        self.0.fmt(f)
    }
}

impl<T: Node> Debug for YangCrummeyQueue<T> {
    fn fmt(&self, f: &mut Formatter) -> ::std::fmt::Result {
        write!(
            f,
            "Head={}, Tail={}",
            self.head_index.load(Ordering::SeqCst),
            self.tail_index.load(Ordering::SeqCst)
        )

    }
}
