// Copyright 2017-2018 the authors. See the 'Copyright and license' section of the
// README.md file at the top-level directory of this repository.
//
// Licensed under the Apache License, Version 2.0 (the LICENSE-APACHE file) or
// the MIT license (the LICENSE-MIT file) at your option. This file may not be
// copied, modified, or distributed except according to those terms.

//! A fast concurrent memory allocator.
//!
//! This module implements a novel memory allocator. The high-level design is inspired
//! by the [`scalloc`][1] allocator, but most of the details are different. We leverage scalable
//! `BagPipe` data-structures to hold available pages, and we use a novel design for individual
//! "spans" of memory (called `Slag`s) that allow for extremely fast remote frees while still
//! facilitating competitive speeds for local allocation and free operations.
//!
//! # `Slag` Lifecycle
//!
//! `Slag`s start as unhewn blocks of memory in a large memory-mapped region called a `Creek`. They
//! are then stored in a `Bagpipe` data-structure (a scalable queue-like object with weak ordering
//! guarantees) in a `PageAlloc` before they are finally initialized by a thread that is able to
//! `claim` it after removing it from the `BagPipe`. `Slag`s that are claimed in this way are said
//! to be in the *owned* state.
//!
//! ## Owned `Slag`s
//!
//! Owned `Slag`s are used to service per-thread allocations for a particuar size class. They are
//! equipped with fast stack and iterator data-structures to allow for fast allocation and
//! deallocation operations that are local to that particular `Slag`. Of course, some `free`
//! operations may correspond to `Slag`s that were not allocated from the `Slag` local to the
//! current thread. These "remote" frees are serviced by an efficient `fetch_or` instruction
//! applied to the remote `slag`'s bit-set. Remote frees also increment a `Slag`-specific reference
//! count.
//!
//! N.B: The reference count and the bit-set cannot be updated atomically, which leads to some
//! subtle code.
//!
//! If a thread is unable to service allocations from its local `Slag`, it attempts to `uncalim` it
//! and get a new `Slag` with more available objects. The `unclaim` protocol is a bit tricky, but
//! if it is successful, the `Slag` is transitioned to the *floating* state.
//!
//! ## Floating, Available and Full `Slag`s
//!
//! A `Slag` is *floating* if it has fewer available objects than some size-class-specific cutoff
//! value. In this state, frees are performed in the same manner as they were in the *owned* state.
//! The difference here is that modifications to a `Slag`'s reference count matter: If a free
//! operation increments the reference count above the cutoff, that thread pushes it to a `BagPipe`
//! containing `Slag`s that can be re-used.
//!
//! `Slag`s in this `BagPipe` are called *available*. From there, they can transition back to the
//! *owned* state. However, what if we have a large number of `Slag`s that are completely full? If
//! they go unused, it makes sense to hand them back to the Operating System.
//!
//! An available `Slag` can be transitioned to a *full* `Slag` if it is present in the available
//! `BagPipe` when it becomes completely full (i.e. all objects from this `Slag` have been freed).
//! If it is present, then it can be *revoked* (i.e. removed in-place) from the `BagPipe` and
//! placed in a global cache of dirty pages (mentioned above). From there it can be uncommitted or
//! cached for use by other object sizes.
//!
//! See the `frontends` module for how the slag subsystem is used to construct allocators.
//!
//! [1]: https://arxiv.org/abs/1503.09006
use std::{cmp, mem};
use std::intrinsics::unlikely;
use std::marker::PhantomData;
use std::ptr::{self, NonNull};
use std::sync::atomic::{fence, AtomicPtr, AtomicUsize, Ordering};

use alloc_fmt::AllocUnwrap;
use bagpipe::bag::{Revocable, WeakBag};
use bagpipe::{BagPipe, BagCleanup};
use bagpipe::queue::RevocableFAAQueue;

use alloc_type::AllocType;
use backing::GCAlloc;
use util::{as_mut, as_ref, mmap, ConfigBuild, LazyGuard, PreDrop, TryCloneWith};

pub type RevocablePipe<T> = BagPipe<RevocableFAAQueue<*mut T>, PageCleanup<T>>;

#[derive(Copy, Clone)]
pub struct PageCleanup<T>(usize, PhantomData<T>);

impl<T> PageCleanup<T> {
    pub fn new(page_size: usize) -> PageCleanup<T> {
        PageCleanup(page_size, PhantomData)
    }
}

impl<T> BagCleanup for PageCleanup<T> {
    type Item = *mut T;
    fn cleanup(&self, it: *mut T) {
        unsafe {
            mmap::unmap(NonNull::new_unchecked(it).cast(), self.0);
        }
    }
}

pub use self::metadata::{Metadata, compute_metadata};

mod metadata {
    use super::*;
    /// Metadata about a particular size-class of objects allocated to a particular page size.
    #[derive(Copy, Clone, Debug, Eq, PartialEq)]
    pub struct Metadata {
        /// The size of an individual object (in bytes).
        pub object_size: usize,

        /// Number of objects in a slab.
        pub n_objects: usize,
        /// Number of `usize` values comprising the bit-set.
        pub n_bitset_words: usize,
        /// Total bytes for the slag.
        pub total_bytes: usize,
        /// Offset (in bytes) from the start of the slag to get the start of the bit-set.
        pub bitset_offset: isize,
        /// Offset (in bytes) from the start of the slag to get to the first object.
        pub objects_offset: isize,
        /// The mask used to initialize a `Slag` bit-set.
        pub object_mask: usize,
        /// The base-2 log of the amount of memory represented by a bit in a bit-set.
        pub bit_rep_shift: usize,
        /// Metadata objects often represent one of several available object sizes. These allocators
        /// are used in concert to provide a general dynamic memory allocator. Threads store pointers
        /// to each of these allocators in an array, this is the index into that array.
        local_index: usize,

        /// A per-allocator tunable that indicates how many available objects a slag must have before
        /// it can be made available to allocating threads.
        pub cutoff_objects: usize,

        /// Size that can be used by this particular object class.
        ///
        /// It may be advantageous to store smaller size classes in a large contiguous unit of memory,
        /// but leave a large portion of that memory unused (with the expectation that it is
        /// uncommited).
        pub usable_size: usize,

        pub ty: AllocType,
    }

    /// Compute an optimal layout for objects of size `obj_size` for `Slag`s of size `page_size` with
    /// cutoff a `cutoff_factor` fraction of total objects, and a local index `local_index`.
    ///
    /// This function essentially performs an exhaustive search over the possible number of objects and
    /// the possible values of `bit_rep_shift` and picks the one with the lowest ratio of unused space,
    /// with a tie-braking preference for fewer words in the `Slag` bit-set.
    pub fn compute_metadata(
        obj_size: usize,
        page_size: usize,
        local_index: usize,
        cutoff_factor: f64,
        usable_size: usize,
        ty: AllocType,
    ) -> Metadata {
        // This is by far the ugliest function in this project. It is all plumbing, heuristics, and
        // other gross things.
        // We start with a bunch of useful helper functions:

        /// Calculate the number of bytes in the bitset needed to represent `n_objects` objects, using
        /// `gran` bits per object.
        fn bitset_bytes(n_objects: usize, gran: usize) -> usize {
            let word_size = mem::size_of::<Word>();
            let word_bits = Word::bits();

            let bits = n_objects * gran;
            let words = if bits % word_bits == 0 {
                bits / word_bits
            } else {
                bits / word_bits + 1
            };
            words * word_size
        }

        /// Calculate the padding (in bytes) required to ensure the `Slag` objects are aligned to
        /// `alignment` bytes, given `n_objects` objects and `gran` bits per object in the bit-set.
        fn align_padding(alignment: usize, n_objects: usize, gran: usize) -> usize {
            alloc_debug_assert!(alignment.is_power_of_two());
            let header_size = slag_size();
            let h_bitset_size = header_size + bitset_bytes(n_objects, gran);
            let rounded = (h_bitset_size + (alignment - 1)) & !(alignment - 1);
            rounded - h_bitset_size
        }

        /// Compute the total bytes used for `n_objects` objects each of size `size` bytes represented
        /// by `gran` bits in the bit-set. This function includes the heuristic that all power-of-two
        /// sizes are aligned to their size, inserting padding accordingly.
        fn total_bytes(size: usize, gran: usize, n_objects: usize) -> usize {
            let header_size = slag_size();
            let padding = if size.is_power_of_two() {
                align_padding(size, n_objects, gran)
            } else {
                0
            };
            header_size + bitset_bytes(n_objects, gran) + padding + n_objects * size
        }

        /// Perform an exhaustive search for the lowest-fragmentation layout of objects of a particular
        /// size, and "shadow size" (i.e. `1 << round_up_to_shift`).
        ///
        /// `cutoff_factor` and `local_index` are just passing through configuration parameters.
        /// `usable_size` is used for smaller object sizes in order for them to use a smaller amount of
        /// memory than the total page size.
        ///
        /// TODO(ezrosent): using a builder for Metadata would clean things up considerably.
        fn meta_inner(
            size: usize,
            page_size: usize,
            round_up_to_shift: usize,
            local_index: usize,
            cutoff_factor: f64,
            usable_size: usize,
        ) -> (f64, usize, Metadata) {
            use std::cmp;
            let usable_size = cmp::min(usable_size, page_size);
            let mut mult = 1.0;
            // we round up to the nearest round_up_to_bytes. As a result, there may be a small amount
            // of padding per-object in the `Slag`.
            let round_up_to_bytes = 1 << round_up_to_shift;
            let padding_per_object = {
                let rem = size % round_up_to_bytes;
                if rem == 0 { 0 } else { round_up_to_bytes - rem }
            };
            let padded_size = size + padding_per_object;
            // gran (read "granularity") is the number of bits used to represent a single object in the
            // slag bit-set.
            let gran = padded_size / round_up_to_bytes;
            alloc_debug_assert!(usable_size > 0);
            alloc_debug_assert!(round_up_to_bytes > 0);
            alloc_debug_assert!(round_up_to_bytes.is_power_of_two());
            alloc_debug_assert!(gran > 0);
            #[cfg_attr(feature = "cargo-clippy", allow(panic_params))]
            alloc_debug_assert!({
                if gran == 1 {
                    padded_size.is_power_of_two()
                } else {
                    true
                }
            });
            // == gran
            // TODO(ezrosent): remove one of these
            let bits_per_object = padded_size >> round_up_to_shift;
            let bits_per_word = Word::bits();
            // Let's say we are storing 24-byte objects with round_up_to_shift=3. This results in each
            // bit in the bit-set representing 8-byte chunks, with each object being 3 bits in the
            // bitset.
            //
            // On 64-bit machines, this means that objects do not fit exactly into a single word: after
            // 21 objects we will have used 63 bits, meaning the 22nd object uses the last bit but is
            // using 2 extra bits in the next bitset word. This number '2' is referred to as "slush".
            //
            // During initialization we perform a word_bits + slush_size (in this case, 66)-bit
            // rotation to compute the masks used for full `Slags`. Part of this is holding the extra
            // "slush" bits in their own mask. This mask is a `usize`, so slush_size cannot exceed
            // word_bits.
            //
            // NB: Having a slush_size greater than the number of bits in a word is a degenerate case,
            // as it implies that more than word_bits bits are required to represent a single object in
            // a bitset. This is essentially never a good idea.
            let slush_size = bits_per_object - (bits_per_word % bits_per_object);
            if bits_per_word < slush_size {
                // give configurations violating this invariant a negative score
                mult = -1.0;
            }
            // First, find out how many objects we can fit while using a maximum of `usable_size`
            let mut n_objects = 1;
            loop {
                if total_bytes(padded_size, gran, n_objects + 1) > usable_size {
                    break;
                }
                n_objects += 1;
            }
            // Get the alignment padding we are using. Note that this is already computed in
            // `total_bytes`, we are just extracting it here.
            let align_padding = if padded_size.is_power_of_two() {
                align_padding(padded_size, n_objects, gran)
            } else {
                0
            };

            // This is takes all of the space we use in this configuration and subtracts all of
            // the "cruft" that isn't used to actually store an object.
            let bs = (total_bytes(padded_size, gran, n_objects) - n_objects * padding_per_object -
                          bitset_bytes(n_objects, gran) - slag_size() -
                          align_padding) as f64;
            let score = if bs > usable_size as f64 { -1.0 } else { 1.0 } * bs /
                (usable_size as f64);
            let header_offset = slag_size() as isize;
            let n_words = bitset_bytes(n_objects, gran) / mem::size_of::<Word>();
            (
                score * mult,
                n_words,
                Metadata {
                    n_objects: n_objects,
                    n_bitset_words: n_words,
                    total_bytes: page_size,
                    bitset_offset: header_offset,
                    objects_offset: header_offset +
                        (align_padding + bitset_bytes(n_objects, gran)) as isize,
                    object_size: padded_size,
                    object_mask: 1,
                    bit_rep_shift: round_up_to_bytes.trailing_zeros() as usize,
                    local_index: local_index,
                    cutoff_objects: cmp::max(1, (n_objects as f64 * cutoff_factor) as usize),
                    usable_size: usable_size,
                    ty: AllocType::SmallSlag,
                },
            )
        }
        let test_meta = Metadata {
            n_objects: 0,
            n_bitset_words: 0,
            total_bytes: 0,
            bitset_offset: 0,
            objects_offset: 0,
            object_size: 0,
            object_mask: 0,
            bit_rep_shift: 0,
            local_index: 0,
            cutoff_objects: 0,
            usable_size: 0,
            ty: AllocType::SmallSlag,
        };

        // now we perform an exhaustive search over these elements.

        // unused in release builds
        #[allow(unused)]
        let (frag, _, mut meta) = (1..(obj_size.next_power_of_two().trailing_zeros() as usize + 1))
            .map(|shift| {
                meta_inner(
                    obj_size,
                    page_size,
                    shift,
                    local_index,
                    cutoff_factor,
                    usable_size,
                )
            })
            .fold((-10.0, 1000, test_meta), |o1, o2| if o1.0 < o2.0 ||
                (o1.0 - o2.0).abs() < 1e-5 &&
                    o1.1 > o2.1
            {
                o2
            } else {
                o1
            });
        // Compute the mask used to represent the first bitset word
        let bits = Word::bits();
        let bits_per_object = meta.object_size >> meta.bit_rep_shift;
        let mut cur_bit = 0;
        let mut mask = 0;
        while cur_bit < bits {
            mask |= 1 << cur_bit;
            cur_bit += bits_per_object;
        }
        meta.object_mask = mask;
        meta.ty = ty;
        trace!("created {:?} fragmentation: {:?}", meta, frag);

        meta
    }

    #[cfg(test)]
    mod tests {
        extern crate env_logger;
        use super::*;

        #[test]
        fn metadata_basic() {
            let _ = env_logger::init();

            compute_metadata(8, 4096, 0, 0.8, 4, AllocType::SmallSlag);
            compute_metadata(16, 4096, 0, 0.8, 1024, AllocType::SmallSlag);
            compute_metadata(24, 4096, 0, 0.8, 1024, AllocType::SmallSlag);
            compute_metadata(127, 4096, 0, 0.8, 1024, AllocType::SmallSlag);
            compute_metadata(800, 2 << 20, 0, 0.8, 32 << 10, AllocType::SmallSlag);
            compute_metadata(514, 4096, 0, 0.8, 1024, AllocType::SmallSlag);
            compute_metadata(513, 2 << 20, 0, 0.8, 1024, AllocType::SmallSlag);
            compute_metadata(768, 4096, 0, 0.8, 1024, AllocType::SmallSlag);
            compute_metadata(800, 4096, 0, 0.8, 32 << 10, AllocType::SmallSlag);
            compute_metadata(1025, 4096, 0, 0.8, 32 << 10, AllocType::SmallSlag);
        }
    }
}

pub use self::bitset::Word;
mod bitset {
    use std::sync::atomic::AtomicUsize;
    use std::mem;
    use std::ops::Deref;
    pub struct Word {
        inner: AtomicUsize, // _padding: [usize; 3],
    }

    impl Word {
        #[inline]
        pub fn bits() -> usize {
            mem::size_of::<usize>() * 8
        }
    }

    impl Deref for Word {
        type Target = AtomicUsize;
        fn deref(&self) -> &AtomicUsize {
            &self.inner
        }
    }
}

pub use self::ref_count::RefCount;
mod ref_count {
    //! Implementation of the `Slag` reference count. This is a
    //! specialized wrapper around an `AtomicUsize` that also reserves a
    //! high-order bit representing whether or not a `Slag` is currently
    //! owned by a thread.
    //!
    //! All `RefCount` methods are documented to mutate the underlying object, despite only
    //! requiring immutable references. This is because all operations are performed atomically.
    use std::sync::atomic::{AtomicUsize, Ordering};
    use std::default::Default;

    #[cfg(target_pointer_width = "32")]
    const WORD_SHIFT: usize = 31;
    #[cfg(target_pointer_width = "64")]
    const WORD_SHIFT: usize = 63;
    const MASK: usize = 1 << WORD_SHIFT;

    pub struct RefCount(AtomicUsize);

    impl Default for RefCount {
        fn default() -> Self {
            RefCount(AtomicUsize::new(0))
        }
    }

    impl RefCount {
        /// Initialize an unclaimed `RefCount` with value `start`.
        pub fn init(&self, start: usize) {
            self.0.store(start, Ordering::Relaxed);
        }

        /// Attempt to claim a `RefCount`.
        pub fn claim(&self) -> bool {
            let was = self.0.fetch_or(MASK, Ordering::Relaxed);
            was & MASK == 0
        }

        /// Unclaim the `RefCount`.
        ///
        /// Returns a tuple whose first element indicates the `RefCount` was previously claimed,
        /// and the second element is the current value of the reference count itself.
        pub fn unclaim(&self) -> (bool, usize) {
            let was = self.0.fetch_and(!MASK, Ordering::Relaxed);
            let claimed = was & MASK == MASK;
            (claimed, was & !MASK)
        }

        /// Increment the `RefCount`.
        ///
        /// Returns a tuple whose first element indicates the `RefCount` was previously claimed,
        /// and the second element is the current value of the reference count itself.
        pub fn inc_n(&self, n: usize) -> (bool, usize) {
            let was = self.0.fetch_add(n, Ordering::Acquire);
            let claimed = was & MASK == MASK;
            (claimed, was & !MASK)
        }

        /// Decrease the `RefCount` by `n`.
        ///
        /// This method assumes the reference count is at least `n`. This is only enforced in debug
        /// builds.
        ///
        /// Returns a tuple whose first element indicates the `RefCount` was previously claimed,
        /// and the second element is the current value of the reference count itself.
        pub fn dec_n(&self, n: usize) -> (bool, usize) {
            let was = self.0.fetch_sub(n, Ordering::Release);
            let claimed = was & MASK == MASK;
            let result = was & !MASK;
            alloc_debug_assert!(
                result >= n,
                "(dec {:?}; claimed={}), was {}, n={}",
                self as *const Self,
                claimed,
                result,
                n
            );
            (claimed, result)
        }

        // keeping this around for debugging purposes
        #[allow(dead_code)]
        pub fn load(&self) -> (bool, usize) {
            let was = self.0.load(Ordering::Acquire);
            let claimed = was & MASK == MASK;
            (claimed, was & !MASK)
        }
    }
}

/// A collection of objects allocated on the heap.
///
/// A `Slag` stores a group of (relatively small) objects of the same size. It includes a pointer to
/// size-class-related `Metadata`, a `RefCount` indicating if the slab is currently claimed by a
/// thread and how many objects remain to be allocated from the `Slag` and a `handle` to facilitate
/// the `Revocable` functionality of `BagPipe`s (used elsewhere).
///
/// Most of a `Slag`'s data are actually not present in the struct definition. The struct fields
/// are more of a *header* that describes the structure of the rest of the `Slag`. A full `Slag`
/// corresponds to some large power-of-two bytes (e.g. the page size). In addition to the header,
/// this is used to store a bit-set which contains information about which elements of the `Slag`
/// are available, along with the objects themselves. For a `Slag` `s`, the bitset corresponds to
/// `s.meta.n_bitset_words` words in memory starting at `s.meta.bitset_offset` bytes from `s`.
/// Similarly, `s.meta.n_objects` are stored contiguously starting at `s.meta.objects_offset`.
#[repr(C)]
pub struct Slag {
    /// Metadata describing how many objects are stored in the `Slag`, the page size, and the
    /// structure of the bitset.
    ty: AllocType,
    meta: AtomicPtr<Metadata>,
    pub rc: RefCount,
    // for BagPipe revocation.
    handle: AtomicUsize,
}

#[inline]
fn slag_size() -> usize {
    cmp::max(mem::size_of::<Slag>(), 64)
}

impl Revocable for Slag {
    fn handle(&self) -> &AtomicUsize {
        &self.handle
    }
}

/// Given an index into a bitset, return the word of the bitset it is in, as well as the bit-wise
/// index into that word to which the item corresponds.
#[cfg_attr(feature = "cargo-clippy", allow(inline_always))]
#[inline(always)]
fn split_index(item_index: usize) -> (isize, usize) {
    // we hand-optimize this to avoid excessively slow debug builds. We call split_index in a very
    // hot path.
    #[cfg(target_pointer_width = "32")]
    const WORD_SHIFT: usize = 5;
    #[cfg(target_pointer_width = "32")]
    const WORD_MASK: usize = 31;
    #[cfg(target_pointer_width = "64")]
    const WORD_SHIFT: usize = 6;
    #[cfg(target_pointer_width = "64")]
    const WORD_MASK: usize = 63;
    ((item_index >> WORD_SHIFT) as isize, item_index & WORD_MASK)
}

/// An iterator over a claimed `Slag`'s bitset.
pub struct AllocIter {
    /// The current word of the bitset being allocated from.
    pub cur_word: usize,
    /// A pointer to the next word that will be consumed, or one-past-the-end of the final word.
    pub next_word: *mut Word,
    /// A pointer to the corresponding `Slag`'s `RefCount`. This allows the iterator to decrement
    /// the `RefCount` when it consumes a new word.
    refcnt: *const RefCount,
    /// The pointer to the beginning of the corresponding `Slag`'s array of objects.
    object_base: *mut u8,
    /// The size of the objects being allocated.
    object_size: usize,
    /// the number of words left to be loaded (not including the currently-loaded one)
    remaining_words: usize,
    /// The index of the current word (starts at zero).
    cur_word_index: usize,
}

impl AllocIter {
    fn new(
        first_bitset_word: *mut Word,
        bitset_words: usize,
        refcnt: *const RefCount,
        object_base: *mut u8,
        object_size: usize,
    ) -> AllocIter {
        unsafe {
            let cur_word = first_bitset_word
                .as_ref()
                .alloc_expect("bitset must point to valid memory")
                .swap(0, Ordering::Acquire);
            (*refcnt).dec_n(cur_word.count_ones() as usize);
            AllocIter {
                cur_word: cur_word,
                next_word: first_bitset_word.offset(1),
                refcnt: refcnt,
                object_base: object_base,
                object_size: object_size,
                remaining_words: (bitset_words - 1),
                cur_word_index: 0,
            }
        }
    }

    /// Acquire a new word from the bit-set.
    ///
    /// Pre-condition: remaining_words > 0.
    /// The key step here is to perform an atomic fetch-and for the value 0. This will give
    /// us a snapshot of available objects corresponding to this word without the risk of losing
    /// any that are concurrently being freed.
    pub unsafe fn refresh_word(&mut self) {
        let next = self.next_word.as_ref().alloc_expect(
            "bitset must point to valid memory",
        );
        self.next_word = self.next_word.offset(1);
        self.cur_word = next.swap(0, Ordering::Acquire);
        (*self.refcnt).dec_n(self.cur_word.count_ones() as usize);
        self.remaining_words -= 1;
        self.cur_word_index += 1;
    }
}

impl Iterator for AllocIter {
    type Item = NonNull<u8>;

    fn next(&mut self) -> Option<NonNull<u8>> {
        let word_size = Word::bits();
        loop {
            let next_bit = self.cur_word.trailing_zeros() as usize;
            unsafe {
                if unlikely(next_bit == word_size) {
                    if self.remaining_words == 0 {
                        return None;
                    }
                    self.refresh_word();
                    continue;
                }
            };
            unsafe {
                self.cur_word ^= 1 << next_bit;
                let object = self.object_base.offset(
                    (self.object_size * (self.cur_word_index * word_size + next_bit)) as
                        isize,
                );
                return Some(NonNull::new_unchecked(object));
            }
        }
    }
}

pub enum Transition {
    Null,
    Available,
    Full,
}

macro_rules! or_slag_word {
    ($slag:expr, $bitset_offset:expr, $word:expr, $mask:expr) => {
        {
            let word = $word;
            (($slag.as_ptr() as *mut u8).offset($bitset_offset) as *mut Word)
                // go to the bitset we want
                .offset(word as isize)
                .as_ref()
                .alloc_expect("slag word null")
                .store($mask, Ordering::Relaxed);
        }
    };
}

impl Slag {
    fn set_metadata(&self, m: *mut Metadata) {
        self.meta.store(m, Ordering::Release);
    }

    pub fn get_metadata(&self) -> &Metadata {
        unsafe {
            self.meta.load(Ordering::Relaxed).as_ref().alloc_expect(
                "metadata should always be non-null",
            )
        }
    }

    pub fn as_raw(&self) -> *mut Self {
        self as *const _ as *mut Self
    }

    /// Initialize the `Slag`.
    ///
    /// This method is called when a new chunk of memory is acquired, *not* when a `Slag` that is
    /// already initialized has been `claim`ed. As such, this simply amounts to initializing all of
    /// the `Slag` data-structures. In order to work in complete generality, the bit-set
    /// initialization is a bit subtle.
    pub unsafe fn init(slag: NonNull<Self>, meta_ptr: NonNull<Metadata>) {
        let slf = as_mut(slag);
        let meta = as_ref(meta_ptr);

        slf.set_metadata(meta_ptr.as_ptr());
        ptr::write(&mut slf.ty, meta.ty);
        slf.rc.init(meta.n_objects);
        slf.handle.store(0, Ordering::Relaxed);
        // This is scaffolding, we perform a slush_size+bits_per_word-bit rotation to compute the
        // mask for each word in the bitset. See the comment in `compute_metadata` for a more
        // detailed example.
        let bits_per_object = meta.object_size >> meta.bit_rep_shift;
        let bits_per_word = Word::bits();
        let slush_size = bits_per_object - (bits_per_word % bits_per_object);
        // this is enforced in compute_metadata
        alloc_debug_assert!(
            bits_per_word >= slush_size,
            "bpw={}, slush_size={} {:?}",
            bits_per_word,
            slush_size,
            meta_ptr
        );

        let end_slush_shift = bits_per_word - slush_size;
        let mut cur_slush = 0;
        let rem = ((meta.n_objects * bits_per_object) % bits_per_word) as u32;
        let rem_mask = !(!0 << rem);
        let mut mask = meta.object_mask;
        if mask == !0 {
            // for all-1s masks, the rotation logic is unnecessary.
            for word in 0..(meta.n_bitset_words - 1) {
                or_slag_word!(slag, meta.bitset_offset, word, !0);
            }
            if rem_mask == 0 {
                or_slag_word!(slag, meta.bitset_offset, meta.n_bitset_words - 1, !0);
            } else {
                or_slag_word!(
                    slag,
                    meta.bitset_offset,
                    meta.n_bitset_words - 1,
                    !0 & rem_mask
                );
            }

            fence(Ordering::Acquire);
            return;
        }
        if rem == 0 {
            for word in 0..(meta.n_bitset_words) {
                or_slag_word!(slag, meta.bitset_offset, word, mask);
                let new_slush = mask >> end_slush_shift;
                mask = mask.wrapping_shl(slush_size as u32);
                // mask <<= slush_size;
                mask |= cur_slush;
                cur_slush = new_slush;
            }
        } else {
            // this is okay, because n_bitset_words must be positive
            for word in 0..(meta.n_bitset_words - 1) {
                or_slag_word!(slag, meta.bitset_offset, word, mask);
                let new_slush = mask >> end_slush_shift;
                mask <<= slush_size;
                mask |= cur_slush;
                cur_slush = new_slush;
            }

            or_slag_word!(
                slag,
                meta.bitset_offset,
                meta.n_bitset_words - 1,
                mask & rem_mask
            );
        }

        fence(Ordering::Acquire);
    }

    /// Given a pointer to an object within a `Slag` with matching `Metadata` find a pointer to the
    /// `Slag`.
    pub fn find(item: NonNull<u8>, alignment: usize) -> NonNull<Self> {
        alloc_debug_assert!(alignment.is_power_of_two());
        alloc_debug_assert!(alignment > 0);
        unsafe { NonNull::new_unchecked(((item.as_ptr() as usize) & !(alignment - 1)) as *mut _) }
    }

    #[inline]
    pub fn get_word(raw_self: NonNull<Slag>, item: NonNull<u8>, m: &Metadata) -> (isize, usize) {
        let it_num = item.as_ptr() as usize;
        let self_num = raw_self.as_ptr() as usize;
        // `Slag` bitsets operate by "pretending" objects are not of the actual object size, but are
        // instead objects of some smaller power-of-2 size. The `bit_rep_shift` value is the base-2
        // log of this "fake size". In order to mark the object as present, we divide by this fake
        // size; which is simply a right-shift by `bit_rep_shift` (hence the name).
        //
        //        item in memory  address of the first object in the slag     / bit rep
        let item_ix = (it_num - ((m.objects_offset as usize) + self_num)) >> m.bit_rep_shift;
        // get the word in the bitset corresponding to the item, as well as the index into that
        // word
        split_index(item_ix)
    }

    /// Free `item` back to this `Slag`.
    ///
    /// This method assumes `item` is a member of `self` (enforced in debug builds). It also
    /// computes whether or not this `free` operation triggered a state transition.
    pub fn dealloc(&self, item: NonNull<u8>) -> Transition {
        let m = self.get_metadata();
        // must be in-bounds
        alloc_debug_assert!((item.as_ptr() as usize) < (self.as_raw() as usize + m.total_bytes));
        let (word, word_ix) = Self::get_word(NonNull::from(self), item, m);
        // first we increment the reference count and then we mark the bitset. Why? During a refill
        // of local state, the bitset _must_ be read first because it informs how much the
        // reference count is incremented. If relaxed ordering were used everywhere, then reference
        // counts could go negative at some point, which would impact correctness given the use of
        // a special "claimed" bit. To avoid a barrier, the least significant bit could be used
        // instead of the MSB, with all inc-s and dec-s being by 2. This is more obvious but
        // removing barriers may be vital on non-intel machines.
        let (claimed, was) = self.rc.inc_n(1);
        unsafe {
            // get the start of the bitset
            ((self.as_raw() as *mut u8).offset(m.bitset_offset) as *mut Word)
                // go to the bitset we want
                .offset(word)
                .as_ref()
                .alloc_unwrap()
                // set the bit in question
                .fetch_or(1 << word_ix, Ordering::Release)
        };
        if !claimed {
            if was == m.cutoff_objects - 1 {
                return Transition::Available;
            }

            if was == m.n_objects - 1 {
                return Transition::Full;
            }
        }
        Transition::Null
    }

    /// Initialize an `AllocIter` for allocating out of the `Slag`.
    pub fn refresh(&self, meta: &Metadata) -> AllocIter {
        // offset calls are valid because size_of(u8) is 1
        unsafe {
            AllocIter::new(
                (self.as_raw() as *mut u8).offset(meta.bitset_offset) as *mut Word,
                meta.n_bitset_words,
                &self.rc,
                (self.as_raw() as *mut u8).offset(meta.objects_offset),
                1 << meta.bit_rep_shift,
            )
        }
    }
}

/// A `DirtyFn` is a callback that is called upon allocating a clean page from a `PageAlloc`. It
/// generally does nothing, but its presence in `PageAlloc` allows us to inject other callbacks for
/// debugging or performance analysis.
pub trait DirtyFn: Clone {
    fn dirty(mem: *mut u8);
}

impl DirtyFn for () {
    #[inline(always)]
    fn dirty(_mem: *mut u8) {}
}

/// Allocator state wrapping a `Slag`.
///
/// This struct forms the "backend" for a particular thread-local cache. It handles the state
/// transitions of different `Slag`s and also acquires new `Slag`s for iteration over the bitset.
pub struct SlagAllocator {
    pub m: NonNull<Metadata>,
    /// The current (local) `Slag`.
    pub slag: NonNull<Slag>,
    /// Available `Slag`s with metadata matching `m`.
    available: RevocablePipe<Slag>,
    /// Uncommit memory for full `Slag`s whose real memory footprint exceeds this threshold.
    eager_decommit_threshold: usize,
    #[cfg(debug_assertions)]
    dropped: bool,
}

#[cfg(debug_assertions)]
impl Drop for SlagAllocator {
    fn drop(&mut self) {
        alloc_debug_assert!(self.dropped);
    }
}

#[derive(Clone)] // so we implement TryClone
pub struct SlagConfig {
    eager_decommit: usize,
    metadata: NonNull<Metadata>,
    // A handle on the bagpipe has to be stored in the configuration because,
    // when a util::Lazy is cloned, only the configuration is stored. Thus, if
    // we only stored the bagpipe handle in the SlagAllocator itself, each time
    // a Lazy<SlagAllocator> was cloned, the clone would have a new bagpipe
    // instead of sharing a bagpipe with the original.
    avail: RevocablePipe<Slag>,
}

impl SlagConfig {
    pub fn new(metadata: NonNull<Metadata>, eager_decommit: usize) -> SlagConfig {
        let page_size = unsafe { metadata.as_ref().total_bytes };
        SlagConfig {
            eager_decommit,
            metadata,
            avail: RevocablePipe::new_size_cleanup(16, PageCleanup::new(page_size)),
        }
    }
}

impl<B: GCAlloc> ConfigBuild<B> for SlagAllocator {
    type Config = SlagConfig;

    fn build(config: &SlagConfig, guard: &LazyGuard, backing: &mut B) -> Option<SlagAllocator> {
        unsafe {
            let first_slag = backing.alloc(guard)?.cast();
            Slag::init(first_slag, config.metadata);
            Some(SlagAllocator {
                m: config.metadata,
                slag: first_slag,
                available: config.avail.clone(),
                eager_decommit_threshold: config.eager_decommit,
                #[cfg(debug_assertions)]
                dropped: false,
            })
        }
    } 
}

unsafe impl Sync for SlagConfig {}
unsafe impl Send for SlagConfig {}

impl SlagAllocator {
    /// Re-initialize a non-empty `AllocIter`; potentially getting a new `Slag` to do so.
    pub unsafe fn refresh<P: GCAlloc>(&mut self, guard: &LazyGuard, pages: &mut P) -> Option<AllocIter> {
        let s_ref = as_ref(self.slag);
        let meta = as_ref(self.m);
        let (_claimed, was) = s_ref.rc.unclaim();
        // We used to have this debug_assert
        //
        // debug_assert!(_claimed, "unclaiming slag on refresh");
        //
        // Why remove it? Because we now call "refresh" on the initial allocation performed
        // up-stream by a cache data-structure. These initial slags will be unclaimed (hence
        // hitting the first branch of the if below). The comment above that branch does not apply
        // to the initialization case.
        alloc_debug_assert_eq!(*meta, *(*s_ref).meta.load(Ordering::Relaxed));

        // There is a race condition between deciding to `unclaim` and actually letting the slag
        // go. This is because only one thread is permitted to transition from unavailable to
        // available: the thread that increments the reference count of the slag past
        // `cutoff_objects`. This transition can only occur when the slag is unclaimed, so we must
        // ensure that the slag's unclaim operation happened on a slag that was below the cutoff.
        //
        // Because we store the claimed bit in the same word as the reference count, it is possible
        // to get a view of both atomically. If we detect that enough objects are available, then
        // we simply re-claim the slag. This guarantees progress because only claimed slags can
        // be allocated from, and no other thread will claim this slag because it has not been
        // added to `available`.
        //
        // It is, however, possible that this slag will be filled up before we can re-claim it. If
        // that happens then our claim operation will still succeed because a successful transition
        // to `full` must successfully revoke the slab from the available bagpipe. But this if
        // condition only evaluates to true if it is impossible to transition the slag to
        // available!
        if was >= meta.cutoff_objects {
            let _claimed = s_ref.rc.claim();
            alloc_debug_assert!(
                _claimed,
                "claiming slag either during initialization or due to being over cutoff"
            );
            Some(s_ref.refresh(meta))
        } else {
            // we need a new slag!
            // first we try and get a slag from the available slagpipe. If it is empty, then we get
            // a fresh page from PageAlloc and initialize it with the current object class's
            // metadata.
            let next_slag = Self::get_slag(&mut self.available, self.m, guard, pages)?;
            self.slag = next_slag;
            let s_ref = self.slag.as_mut();
            let claimed = s_ref.rc.claim();
            alloc_debug_assert!(claimed, "claiming new slag after refresh");
            Some(s_ref.refresh(meta))
        }
    }

    fn get_slag<P: GCAlloc>(available: &mut RevocablePipe<Slag>, meta: NonNull<Metadata>, guard: &LazyGuard, pages: &mut P) -> Option<NonNull<Slag>> {
        unsafe {
            available.try_pop_mut(guard.guard())
                .map(|slag| {
                    trace_event!(grabbed_available);
                    NonNull::new_unchecked(slag)
                })
                .ok()
                .or_else(|| {
                    // If we can't allocate backing memory for a new slag, then
                    // we're OOM.
                    let new_raw: NonNull<Slag> = pages.alloc(guard)?.cast();
                    // TODO: If this check succeeds, are we guaranteed to have a
                    // valid, uncorrupted slag? Does it depend on the implementation
                    // of P?
                    if new_raw.as_ref().meta.load(Ordering::Relaxed) != meta.as_ptr() {
                        Slag::init(new_raw, meta);
                    }
                    Some(new_raw)
                })
        }
    }

    fn transition_available(&mut self, guard: &LazyGuard, slag: NonNull<Slag>) {
        trace_event!(transition_available);
        self.available.push_mut(guard.guard(), slag.as_ptr())
    }

    #[cfg_attr(feature = "cargo-clippy", allow(inline_always))]
    #[inline(always)]
    unsafe fn transition_full<P: GCAlloc>(&mut self, guard: &LazyGuard, pages: &mut P, slag: NonNull<Slag>) {
        if RevocablePipe::revoke(&slag.as_ptr()) {
            slag.as_ref().handle.store(0, Ordering::Release);
            trace_event!(transition_full);
            pages.dealloc(guard, slag.cast())
        }
        // Otherwise caught in a strange race condition (see comments in alloc). We can
        // safely return without further work.
    }

    pub unsafe fn bulk_dealloc<P: GCAlloc>(
        &mut self,
        guard: &LazyGuard,
        pages: &mut P,
        mask: usize,
        word: NonNull<Word>,
        slag: NonNull<Slag>,
        meta: &Metadata,
    ) {
        let n_ones = mask.count_ones() as usize;
        if n_ones == 0 {
            return;
        }
        trace_event!(bulk_remote_free);
        let s_ref = slag.as_ref();
        let (claimed, was) = s_ref.rc.inc_n(n_ones);
        let before = word.as_ref().fetch_or(mask, Ordering::Release);
        alloc_debug_assert_eq!(
            before & mask,
            0,
            "\nInvalid mask (obj size {:?}): transitioned\n{:064b} with \n{:064b}",
            meta.object_size,
            before,
            mask,
        );
        let now = was + n_ones;
        if !claimed {
            if now == meta.n_objects {
                self.transition_full(guard, pages, slag);
            } else if was < meta.cutoff_objects && now >= meta.cutoff_objects {
                self.transition_available(guard, slag);
            }
        }
    }

    /// Perform a "remote" free to the `Slag` containing `item`.
    pub unsafe fn dealloc<P: GCAlloc>(&mut self, guard: &LazyGuard, pages: &mut P, item: NonNull<u8>) {
        trace_event!(remote_free);
        let meta = as_ref(self.m);
        let it_slag = Slag::find(item, meta.total_bytes);
        match it_slag.as_ref().dealloc(item) {
            Transition::Null => return,
            Transition::Available => self.transition_available(guard, it_slag),
            Transition::Full => self.transition_full(guard, pages, it_slag),
        }
    }

    /// Test if `it` is an element of the current `Slag`.
    pub fn contains(&self, it: NonNull<u8>) -> bool {
        unsafe {
            let meta = as_ref(self.m);
            let it_slag = Slag::find(it, meta.total_bytes);
            it_slag == self.slag
        }
    }
}

impl<B: GCAlloc> TryCloneWith<B> for SlagAllocator {
    fn try_clone_with(&self, guard: &LazyGuard, backing: &mut B) -> Option<Self> {
        // TODO: Avoid leaking resources on failure (see issue #179)
        let mut available = self.available.clone();
        let slag = Self::get_slag(&mut available, self.m, guard, backing)?;
        Some(SlagAllocator {
            m: self.m,
            slag,
            available,
            eager_decommit_threshold: self.eager_decommit_threshold,
            #[cfg(debug_assertions)]
            dropped: false,
        })
    }
}

unsafe impl<B: GCAlloc> PreDrop<B> for SlagAllocator {
    unsafe fn pre_drop(&mut self, guard: &LazyGuard, backing: &mut B) {
        #[cfg(debug_assertions)]
        {
            alloc_debug_assert!(!self.dropped);
            self.dropped = true;
        }
        let slag = self.slag;
        let meta = as_ref(self.m);
        let (claimed, was) = slag.as_ref().rc.unclaim();
        if claimed {
            // we used this slag at some point
            if was == meta.n_objects {
                backing.dealloc(guard, slag.cast());
                trace_event!(transition_full);
            // self.transition_full(slag, meta)
            } else if was >= meta.cutoff_objects {
                self.transition_available(guard, slag)
            }
        } else {
            // we never allocated from this slag, so just free it back to the page allocator
            backing.dealloc(guard, slag.cast());
        }
    }
}
