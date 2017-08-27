// Copyright 2017 the authors. See the 'Copyright and license' section of the
// README.md file at the top-level directory of this repository.
//
// Licensed under the Apache License, Version 2.0 (the LICENSE file). This file
// may not be copied, modified, or distributed except according to those terms.

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
//! [1]: https://arxiv.org/abs/1503.09006
use std::mem;
use std::sync::Arc;
use std::sync::atomic::{fence, Ordering, AtomicUsize, AtomicPtr};
use super::bagpipe::bag::{WeakBag, Revocable};
use super::bagpipe::BagPipe;
use super::bagpipe::queue::{RevocableFAAQueue, FAAQueueLowLevel};
use super::utils::{LazyInitializable, OwnedArray, mmap};
use std::marker::PhantomData;
use std::ptr;
use std::cmp;

#[cfg(feature = "nightly")]
use std::intrinsics::{unlikely, likely};

#[cfg(not(feature = "nightly"))]
#[cfg_attr(feature = "cargo-clippy", allow(inline_always))]
#[inline(always)]
unsafe fn likely(b: bool) -> bool {
    b
}

#[cfg(not(feature = "nightly"))]
#[cfg_attr(feature = "cargo-clippy", allow(inline_always))]
#[inline(always)]
unsafe fn unlikely(b: bool) -> bool {
    b
}

type SlagPipe<T> = BagPipe<FAAQueueLowLevel<*mut T>>;
pub type RevocablePipe<T> = BagPipe<RevocableFAAQueue<*mut T>>;

/// A generator of chunks of memory providing an `sbrk`-like interface.
pub trait MemoryBlock
    where Self: Clone
{
    fn new(page_size: usize) -> Self;
    /// The smallest unit of memory that can be `carve`d.
    fn page_size(&self) -> usize;
    /// Is `it` a pointer to somewhere in the block of memory.
    fn contains(&self, it: *mut u8) -> bool;
    /// Return `npages` fresh pages from the `Creek`.
    ///
    /// Currently, there is code in this module (see the `Coalescer`) that relies on fresh pages
    /// returned from `carve` to be filled with zeros.
    fn carve(&self, npages: usize) -> *mut u8;
}

/// An allocator that allocates objects at the granularity of the page size of the underlying
/// `MemoryBlock`.
pub trait CoarseAllocator
    where Self: Clone
{
    /// The concrete type representing backing memory for the allocator.
    type Block: MemoryBlock;

    /// The start of a new block of memory of size `backing_memory().page_size()`.
    ///
    /// Furthermore, all memory returned by `alloc` must satisfy
    /// `c.backing_memory().contains(c.alloc())`*.
    ///
    /// *That is, if that code actually compiled and didn't have a lifetime issue.
    unsafe fn alloc(&mut self) -> *mut u8;

    /// Free a page of memory back to the allocator.
    ///
    /// If `item` is not contained in `self.backing_memory()`, the behavior of `free` is undefined.
    /// The `uncommit` flag is a hint to the allocator to uncommit the memory. It need not be
    /// observed.
    unsafe fn free(&mut self, item: *mut u8, uncommit: bool);

    /// Get access to the backing memory for the allocator.
    fn backing_memory(&self) -> &Self::Block;
}

/// Metadata about a particular size-class of objects allocated to a particular page size.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct Metadata {
    /// The size of an individual object (in bytes).
    pub object_size: usize,

    /// Number of objects in a slab.
    n_objects: usize,
    /// Number of `usize` values comprising the bit-set.
    n_bitset_words: usize,
    /// Total bytes for the slag.
    total_bytes: usize,
    /// Offset (in bytes) from the start of the slag to get the start of the bit-set.
    bitset_offset: isize,
    /// Offset (in bytes) from the start of the slag to get to the first object.
    objects_offset: isize,
    /// The mask used to initialize a `Slag` bit-set.
    object_mask: usize,
    /// The base-2 log of the amount of memory represented by a bit in a bit-set.
    bit_rep_shift: usize,
    /// Metadata objects often represent one of several available object sizes. These allocators
    /// are used in concert to provide a general dynamic memory allocator. Threads store pointers
    /// to each of these allocators in an array, this is the index into that array.
    local_index: usize,

    /// A per-allocator tunable that indicates how many available objects a slag must have before
    /// it can be made available to allocating threads.
    cutoff_objects: usize,

    /// Size that can be used by this particular object class.
    ///
    /// It may be advantageous to store smaller size classes in a large contiguous unit of memory,
    /// but leave a large portion of that memory unused (with the expectation that it is
    /// uncommited).
    usable_size: usize,
}

use self::bitset::Word;
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

use self::ref_count::RefCount;
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

    #[cfg(target_pointer_width="32")]
    const WORD_SHIFT: usize = 31;
    #[cfg(target_pointer_width="64")]
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
            debug_assert!(result >= n,
                          "(dec {:?}; claimed={}), was {}, n={}",
                          self as *const Self,
                          claimed,
                          result,
                          n);
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
pub struct Slag {
    /// Metadata describing how many objects are stored in the `Slag`, the page size, and the
    /// structure of the bitset.
    meta: AtomicPtr<Metadata>,
    rc: RefCount,
    // for BagPipe revocation.
    handle: AtomicUsize,
    _padding: [usize; 5],
}

impl Revocable for Slag {
    fn handle(&self) -> &AtomicUsize {
        &self.handle
    }
}

/// Compute an optimal layout for objects of size `obj_size` for `Slag`s of size `page_size` with
/// cutoff a `cutoff_factor` fraction of total objects, and a local index `local_index`.
///
/// This function essentially performs an exhaustive search over the possible number of objects and
/// the possible values of `bit_rep_shift` and picks the one with the lowest ratio of unused space,
/// with a tie-braking preference for fewer words in the `Slag` bit-set.
pub fn compute_metadata(obj_size: usize,
                        page_size: usize,
                        local_index: usize,
                        cutoff_factor: f64,
                        usable_size: usize)
                        -> Metadata {
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
        debug_assert!(alignment.is_power_of_two());
        let header_size = mem::size_of::<Slag>();
        let h_bitset_size = header_size + bitset_bytes(n_objects, gran);
        let rounded = (h_bitset_size + (alignment - 1)) & !(alignment - 1);
        rounded - h_bitset_size
    }

    /// Compute the total bytes used for `n_objects` objects each of size `size` bytes represented
    /// by `gran` bits in the bit-set. This function includes the heuristic that all power-of-two
    /// sizes are aligned to their size, inserting padding accordingly.
    fn total_bytes(size: usize, gran: usize, n_objects: usize) -> usize {
        let header_size = mem::size_of::<Slag>();
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
    fn meta_inner(size: usize,
                  page_size: usize,
                  round_up_to_shift: usize,
                  local_index: usize,
                  cutoff_factor: f64,
                  usable_size: usize)
                  -> (f64, usize, Metadata) {
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
        debug_assert!(usable_size > 0);
        debug_assert!(round_up_to_bytes > 0);
        debug_assert!(round_up_to_bytes.is_power_of_two());
        debug_assert!(gran > 0);
        #[cfg_attr(feature = "cargo-clippy", allow(panic_params))]
        debug_assert!({
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
                  bitset_bytes(n_objects, gran) - mem::size_of::<Slag>() -
                  align_padding) as f64;
        let score = if bs > usable_size as f64 { -1.0 } else { 1.0 } * bs / (usable_size as f64);
        let header_offset = mem::size_of::<Slag>() as isize;
        let n_words = bitset_bytes(n_objects, gran) / mem::size_of::<Word>();
        (score * mult,
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
         })
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
    };

    // now we perform an exhaustive search over these elements.

    // unused in release builds
    #[allow(unused)]
    let (frag, _, mut meta) = (1..(obj_size.next_power_of_two().trailing_zeros() as usize + 1))
        .map(|shift| {
            meta_inner(obj_size,
                       page_size,
                       shift,
                       local_index,
                       cutoff_factor,
                       usable_size)
        })
        .fold((-10.0, 1000, test_meta),
              |o1, o2| if o1.0 < o2.0 || (o1.0 - o2.0).abs() < 1e-5 && o1.1 > o2.1 {
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
    trace!("created {:?} fragmentation: {:?}", meta, frag);
    meta
}

/// Given an index into a bitset, return the word of the bitset it is in, as well as the bit-wise
/// index into that word to which the item corresponds.
#[cfg_attr(feature="cargo-clippy", allow(inline_always))]
#[inline(always)]
fn split_index(item_index: usize) -> (isize, usize) {
    // we hand-optimize this to avoid excessively slow debug builds. We call split_index in a very
    // hot path.
    #[cfg(target_pointer_width="32")]
    const WORD_SHIFT: usize = 5;
    #[cfg(target_pointer_width="32")]
    const WORD_MASK: usize = 31;
    #[cfg(target_pointer_width="64")]
    const WORD_SHIFT: usize = 6;
    #[cfg(target_pointer_width="64")]
    const WORD_MASK: usize = 63;
    ((item_index >> WORD_SHIFT) as isize, item_index & WORD_MASK)
}

/// An iterator over a claimed `Slag`'s bitset.
struct AllocIter {
    /// The current word of the bitset being allocated from.
    cur_word: usize,
    /// A pointer to the next word that will be consumed, or one-past-the-end of the final word.
    next_word: *mut Word,
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
    fn new(first_bitset_word: *mut Word,
           bitset_words: usize,
           refcnt: *const RefCount,
           object_base: *mut u8,
           object_size: usize)
           -> AllocIter {
        unsafe {
            let cur_word = first_bitset_word.as_ref()
                .expect("bitset must point to valid memory")
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
    fn refresh_word(&mut self) {
        unsafe {
            let next = self.next_word
                .as_ref()
                .expect("bitset must point to valid memory");
            self.next_word = self.next_word.offset(1);
            self.cur_word = next.swap(0, Ordering::Acquire);
            (*self.refcnt).dec_n(self.cur_word.count_ones() as usize);
            self.remaining_words -= 1;
            self.cur_word_index += 1;
        }
    }
}

impl Iterator for AllocIter {
    type Item = *mut u8;

    fn next(&mut self) -> Option<*mut u8> {
        let word_size = Word::bits();
        loop {
            let next_bit = self.cur_word.trailing_zeros() as usize;
            if unsafe { unlikely(next_bit == word_size) } {
                if self.remaining_words == 0 {
                    return None;
                }
                self.refresh_word();
                continue;
            }
            unsafe {
                self.cur_word ^= 1 << next_bit;
                let object = self.object_base
                    .offset((self.object_size * (self.cur_word_index * word_size + next_bit)) as
                            isize);
                return Some(object);
            }
        }
    }
}

enum Transition {
    Null,
    Available,
    Full,
}

macro_rules! or_slag_word {
        ($slag:expr, $bitset_offset:expr, $word:expr, $mask:expr) => {
            {
                let word = $word;
                (($slag as *mut u8).offset($bitset_offset) as *mut Word)
                    // go to the bitset we want
                    .offset(word as isize)
                    .as_ref()
                    .unwrap()
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
            self.meta
                .load(Ordering::Relaxed)
                .as_ref()
                .expect("metadata should always be non-null")
        }
    }

    fn as_raw(&self) -> *mut Self {
        self as *const _ as *mut Self
    }

    /// Initialize the `Slag`.
    ///
    /// This method is called when a new chunk of memory is acquired, *not* when a `Slag` that is
    /// already initialized has been `claim`ed. As such, this simply amounts to initializing all of
    /// the `Slag` data-structures. In order to work in complete generality, the bit-set
    /// initialization is a bit subtle.
    unsafe fn init(slag: *mut Self, meta: &Metadata) {
        let slf = slag.as_mut().unwrap();
        slf.set_metadata(meta as *const _ as *mut Metadata);
        slf.rc.init(meta.n_objects);
        slf.handle.store(0, Ordering::Relaxed);
        // This is scaffolding, we perform a slush_size+bits_per_word-bit rotation to compute the
        // mask for each word in the bitset. See the comment in `compute_metadata` for a more
        // detailed example.
        let bits_per_object = meta.object_size >> meta.bit_rep_shift;
        let bits_per_word = Word::bits();
        let slush_size = bits_per_object - (bits_per_word % bits_per_object);
        // this is enforced in compute_metadata
        debug_assert!(bits_per_word >= slush_size,
                      "bpw={}, slush_size={}",
                      bits_per_word,
                      slush_size);
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
                or_slag_word!(slag,
                              meta.bitset_offset,
                              meta.n_bitset_words - 1,
                              !0 & rem_mask);
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

            or_slag_word!(slag,
                          meta.bitset_offset,
                          meta.n_bitset_words - 1,
                          mask & rem_mask);
        }

        fence(Ordering::Acquire);
    }

    /// Given a pointer to an object within a `Slag` with matching `Metadata` find a pointer to the
    /// `Slag`.
    pub fn find(item: *mut u8, alignment: usize) -> *mut Self {
        debug_assert!(alignment.is_power_of_two());
        debug_assert!(alignment > 0);
        ((item as usize) & !(alignment - 1)) as *mut Self
    }

    #[inline]
    fn get_word(raw_self: *mut Slag, item: *mut u8, m: &Metadata) -> (isize, usize) {
        let it_num = item as usize;
        let self_num = raw_self as usize;
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
    fn free(&self, item: *mut u8) -> Transition {
        let m = self.get_metadata();
        // must be in-bounds
        debug_assert!((item as usize) < (self.as_raw() as usize + m.total_bytes));
        let (word, word_ix) = Self::get_word(self.as_raw(), item, m);
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
                .unwrap()
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
    fn refresh(&self, meta: &Metadata) -> AllocIter {
        // offset calls are valid because size_of(u8) is 1
        unsafe {
            AllocIter::new((self.as_raw() as *mut u8).offset(meta.bitset_offset) as *mut Word,
                           meta.n_bitset_words,
                           &self.rc,
                           (self.as_raw() as *mut u8).offset(meta.objects_offset),
                           1 << meta.bit_rep_shift)
        }

    }
}

/// A set data-structure used to batch remote free operations.
struct Coalescer(OwnedArray<RemoteFreeCell>, PtrStack);

/// The internal data for a `Coalescer`.
///
/// This holds all the provenance necessary to perform a batched remote free operation.
struct RemoteFreeCell {
    // TODO: explore using slag-specific information as well as word-specific.
    // Idea: have the "key" be rc, and then use a *mut Slag to get at the entire bitset, along with
    // an array of masks, then you can go as before, but only increment ref count once.
    /// The reference count to be updated in accordance for the accumulated `mask`.
    rc: *mut RefCount,
    /// The specific bitset word to be or-ed with `mask`.
    word: *mut Word,
    /// The accumulated mask to update `word`.
    mask: usize,
}

impl Default for RemoteFreeCell {
    fn default() -> Self {
        RemoteFreeCell {
            rc: ptr::null_mut(),
            word: ptr::null_mut(),
            mask: 0,
        }
    }
}

impl Coalescer {
    fn new(size: usize) -> Self {
        Coalescer(OwnedArray::new(size.next_power_of_two()),
                  PtrStack::new(size))
    }

    fn bucket_num(&self, word: usize) -> usize {
        // we can do a "fast mod" operation because we know len() is a power of two (see `new`)
        word as usize & (self.0.len() - 1)
    }

    /// Try to insert `item`.
    ///
    /// The return value indicates if the value was successfully inserted.
    unsafe fn insert(&mut self, item: *mut u8, meta: &Metadata) -> bool {
        let s = &*Slag::find(item, meta.total_bytes);
        let rc_ptr = &s.rc as *const _ as *mut RefCount;
        let (word, word_ix) = Slag::get_word(s.as_raw(), item, meta);
        let word_ptr = ((s.as_raw() as *mut u8).offset(meta.bitset_offset) as *mut Word)
            .offset(word);
        let bucket_ind = self.bucket_num(word_ptr as usize >> 3);
        let bucket = &mut *self.0.get(bucket_ind);
        // XXX: using the property of the creek implementation that it fills newly-dirtied pages
        // with zeros.
        if bucket.rc.is_null() {
            *bucket = RemoteFreeCell {
                rc: rc_ptr,
                word: word_ptr,
                mask: 1 << word_ix,
            };
            // append this to a stack of buckets we know are non-null
            self.1.push(bucket as *const _ as *mut u8);
            return true;
        }

        if bucket.word == word_ptr {
            bucket.mask |= 1 << word_ix;
            return true;
        }
        false
    }
}

/// A different approach to caching to `LocalCache` inspired by Bonwick-style magazines.
///
/// The advantage `MagazineCache` has over `LocalCache` is that it will unconditionally push to a
/// local data-structure if there is room (it doesn't have to be a local free). The magazine
/// structure also allows us to batch together remote frees (by simply composing bit-set masks
/// ahead of a fetch-or), reducing the number of atomic instruction that must be issued for most
/// remote frees.
pub struct MagazineCache<CA: CoarseAllocator> {
    stack_size: usize,
    s: PtrStack,
    iter: AllocIter,
    alloc: SlagAllocator<CA>,
    coalescer: Coalescer,
    flag: usize,
}

impl<CA: CoarseAllocator> LazyInitializable for MagazineCache<CA> {
    type Params = (*mut Metadata, usize, CA, RevocablePipe<Slag>);
    fn init(&(meta, decommit, ref page_alloc, ref avail): &Self::Params) -> Self {
        let salloc = SlagAllocator::partial_new(meta, decommit, page_alloc.clone(), avail.clone());
        Self::new(salloc)
    }
}

impl<CA: CoarseAllocator> LazyInitializable for LocalCache<CA> {
    type Params = (*mut Metadata, usize, CA, RevocablePipe<Slag>);
    fn init(&(meta, decommit, ref page_alloc, ref avail): &Self::Params) -> Self {
        let salloc = SlagAllocator::partial_new(meta, decommit, page_alloc.clone(), avail.clone());
        Self::new(salloc)
    }
}

impl<CA: CoarseAllocator> Drop for MagazineCache<CA> {
    fn drop(&mut self) {
        unsafe {
            let meta = &*self.alloc.m;
            let mask = self.iter.cur_word;
            let word = self.iter.next_word.offset(-1);
            let slag = self.alloc.slag;
            // bulk-free the current AllocIter word. Then free all elements in the magazine.
            self.alloc.bulk_free(mask, word, slag, meta);
            for i in 0..self.s.top {
                let item = *self.s.data.get(i);
                self.alloc.free(item)
            }
        }
    }
}

impl<CA: CoarseAllocator> Clone for MagazineCache<CA> {
    fn clone(&self) -> Self {
        MagazineCache::new_sized(self.alloc.clone(), self.stack_size)
    }
}

impl<CA: CoarseAllocator> MagazineCache<CA> {
    pub fn new_sized(mut alloc: SlagAllocator<CA>, magazine_size: usize) -> Self {
        assert!(magazine_size > 0);
        let s = PtrStack::new(magazine_size);
        let iter = unsafe { alloc.refresh() };
        let buckets = Coalescer::new(magazine_size * 4);
        MagazineCache {
            stack_size: magazine_size,
            s: s,
            iter: iter,
            alloc: alloc,
            coalescer: buckets,
            flag: 0,
        }
    }

    pub fn new(alloc: SlagAllocator<CA>) -> Self {
        use std::cmp;
        unsafe {
            const DEFAULT_MAGAZINE_BYTES: usize = 512 << 10; // 1 << 20;
            let sz = DEFAULT_MAGAZINE_BYTES / (*alloc.m).object_size;
            Self::new_sized(alloc, cmp::max(1, sz))
        }
    }

    /// Allocate memory from the current owned `Slag`.
    ///
    /// This amounts to getting memory from the current alloc iterator. If the iterator is
    /// exhausted, a new `Slag` is acquired.
    unsafe fn slag_alloc(&mut self) -> *mut u8 {
        for _ in 0..2 {
            match self.iter.next() {
                Some(ptr) => return ptr,
                None => self.iter = self.alloc.refresh(),
            }
        }
        panic!("New slag is empty {:?} {:?}",
               self.alloc.slag,
               (*self.alloc.slag).rc.load())
    }

    pub unsafe fn alloc(&mut self) -> *mut u8 {
        if let Some(ptr) = self.s.pop() {
            trace_event!(cache_alloc);
            ptr
        } else {
            trace_event!(slag_alloc);
            self.slag_alloc()
        }
    }

    pub unsafe fn free(&mut self, item: *mut u8) {
        self.flag += 1;
        self.flag &= 31;
        if likely(self.flag > 0) {
            trace_event!(local_free);
            if self.s.top < self.stack_size {
                self.s.push(item);
                return;
            }
            self.return_memory();
            self.s.push(item);
        } else {
            self.alloc.free(item)
        }
    }

    /// Perform the bulk-level frees for the `Coalescer`.
    unsafe fn return_memory(&mut self) {
        debug_assert_eq!(self.s.top as usize, self.stack_size);
        let meta = &*self.alloc.m;
        // iterate over the stack and attempt to add them to the coalescer.
        for i in 0..self.stack_size {
            let item = *self.s.data.get(i);
            if !self.coalescer.insert(item, meta) {
                // there was a "hash collision", so we simply free `item` directly
                self.alloc.free(item)
            }
        }
        self.s.top = 0;
        for cell_ptr in 0..self.coalescer.1.top {
            let cell = &mut **(self.coalescer.1.data.get(cell_ptr) as *mut *mut RemoteFreeCell);
            // Slag::find will technically work if you hand it any pointer within the slag
            // itself, not just an object. As a result, we use the reference count to get at
            // the slag it belongs to.
            let slag = Slag::find(cell.rc as *mut u8, meta.total_bytes);
            self.alloc.bulk_free(cell.mask, cell.word, slag, meta);
            ptr::write(cell, RemoteFreeCell::default());
        }
        self.coalescer.1.top = 0;
    }
}

/// A `LocalCache` provides thread-local data on top of a `SlagAllocator`.
///
/// Like a `MagazineCache`, it includes a stack of pointers to cache allocations. Unlike
/// `MagazineCache`, only pointers local to the current `Slag` are placed in this stack: the rest
/// are eagerly freed back to their owning `Slag`. Generally speaking, `LocalCache`s are better at
/// producer-consumer workloads and worse at predominantly thread-local workloads than the
/// `MagazineCache`. `LocalCache`s also have stronger guarantees when it comes to object locality:
/// almost all adjacent allocations will come from the same region of memory.
pub struct LocalCache<CA: CoarseAllocator> {
    alloc: SlagAllocator<CA>,
    vals: PtrStack,
    iter: AllocIter,
}

impl<CA: CoarseAllocator> Drop for LocalCache<CA> {
    fn drop(&mut self) {
        unsafe {
            let meta = &*self.alloc.m;
            let mask = self.iter.cur_word;
            let word = self.iter.next_word.offset(-1);
            let slag = self.alloc.slag;
            self.alloc.bulk_free(mask, word, slag, meta);
            for i in 0..self.vals.top {
                let item = *self.vals.data.get(i);
                self.alloc.free(item)
            }
        }
    }
}
impl<CA: CoarseAllocator> Clone for LocalCache<CA> {
    fn clone(&self) -> LocalCache<CA> {
        LocalCache::new(self.alloc.clone())
    }
}

impl<CA: CoarseAllocator> LocalCache<CA> {
    fn new(mut alloc: SlagAllocator<CA>) -> Self {
        unsafe {
            let stack = PtrStack::new((*alloc.m).n_objects);
            let iter = alloc.refresh();
            LocalCache {
                alloc: alloc,
                vals: stack,
                iter: iter,
            }
        }
    }

    pub unsafe fn free(&mut self, it: *mut u8) {
        if self.alloc.contains(it) {
            self.vals.push(it);
        } else {
            self.alloc.free(it);
        }
    }

    pub unsafe fn alloc(&mut self) -> *mut u8 {
        self.vals
            .pop()
            .or_else(|| self.iter.next())
            .unwrap_or_else(|| {
                let next_iter = self.alloc.refresh();
                self.iter = next_iter;
                self.iter
                    .next()
                    .expect("New iterator should have values")
            })
    }
}

/// Base address and size of a memory map.
///
/// This could also just be a `*mut [u8]`, but having two fields is more explicit. We need a new
/// type because the `Drop` implementation calls `unmap`.
#[derive(Debug)]
struct MapAddr(*mut u8, usize);

impl Drop for MapAddr {
    fn drop(&mut self) {
        use self::mmap::unmap;
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
    page_size: usize,
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

impl MemoryBlock for Creek {
    #[inline]
    fn page_size(&self) -> usize {
        check_bump!(self);
        self.page_size
    }

    fn carve(&self, npages: usize) -> *mut u8 {
        check_bump!(self);
        unsafe {
            let new_bump = self.bump
                .load(Ordering::Relaxed)
                .as_ref()
                .unwrap()
                .fetch_add(npages, Ordering::Relaxed);
            debug_assert!((new_bump * (self.page_size + 1)) < self.map_info.1);
            self.base.offset((new_bump * self.page_size) as isize)
        }
    }

    fn contains(&self, it: *mut u8) -> bool {
        check_bump!(self);
        let it_num = it as usize;
        let base_num = self.base as usize;
        it_num >= base_num && it_num < base_num + self.map_info.1
    }

    /// Create a new `Creek` with pages of size `page_size` total heap size of `heap_size`,
    /// optionally backed by huge pages.
    ///
    /// Page size and heap size should be powers of two. The allocator may want to reserve some
    /// pages for itself (or for alignment reasons), as a result it is a good idea to have
    /// heap_size be much larger than page_size.
    fn new(page_size: usize) -> Self {
        use self::mmap::fallible_map;
        let get_heap = || {
            let mut heap_size: usize = 2 << 40;
            while heap_size > (1 << 30) {
                if let Some(heap) = fallible_map(heap_size) {
                    return (heap, heap_size);
                }
                heap_size /= 2;
            }
            panic!("unable to map heap")
        };
        // lots of stuff breaks if this isn't true
        assert!(page_size.is_power_of_two());
        assert!(page_size > mem::size_of::<usize>());
        // first, let's grab some memory;
        let (orig_base, heap_size) = get_heap();
        info!("created heap of size {}", heap_size);
        let orig_addr = orig_base as usize;
        let (slush_addr, real_addr) = {
            // allocate some `slush` space at the beginning of the creek. This gives us space to
            // store the `bump` pointer. In the future, we may store more things in this slush
            // space as well.
            //
            // In addition, we must ensure that pages are aligned to their size.
            let base = if orig_addr == 0 {
                // this is a real possibility if we are calling `mmap` directly.
                // However, `MmapAlloc` currently handles `mmap` returning null, so this is
                // technically a redundant check.
                orig_addr + page_size
            } else if orig_addr % page_size != 0 {
                let rem = orig_addr % page_size;
                orig_addr + (page_size - rem)
            } else {
                orig_addr
            };
            (base as *mut u8, (base + page_size) as *mut u8)
        };
        Creek {
            page_size: page_size,
            map_info: Arc::new(MapAddr(orig_base, heap_size)),
            base: real_addr,
            bump: AtomicPtr::new(slush_addr as *mut AtomicUsize),
        }
    }
}

impl Clone for Creek {
    fn clone(&self) -> Self {
        let bump = self.bump.load(Ordering::Relaxed);
        debug_assert!(!bump.is_null());
        Creek {
            page_size: self.page_size,
            map_info: self.map_info.clone(),
            base: self.base,
            bump: AtomicPtr::new(bump),
        }
    }
}

pub trait DirtyFn: Clone {
    fn dirty(mem: *mut u8);
}

impl DirtyFn for () {
    #[inline(always)]
    fn dirty(_mem: *mut u8) {}
}

/// An allocator for large, fixed-sized objects.
///
/// A `PageAlloc` is essentially a cache of pages sitting in front of a `Creek`. It keeps track of
/// which pages are clean and which are potentially dirty, and it will uncommit dirty pages if it
/// notices that there are too many.
///
/// The use of `BagPipe` data-structures allows the `PageAlloc` to scale to many concurrent
/// allocating and freeing threads.
///
/// TODO: implement a threshold for eager uncommit in the `SlagAllocator` and propagate that to
/// `CoarseAllocator`
#[derive(Clone)]
pub struct PageAlloc<C: MemoryBlock, D = ()>
    where D: DirtyFn
{
    target_overhead: usize,
    creek: C,
    // bagpipes of byte slices of size creek.page_size
    clean: SlagPipe<u8>,
    dirty: SlagPipe<u8>,
    _marker: PhantomData<D>,
}

impl<C: MemoryBlock, D: DirtyFn> PageAlloc<C, D> {
    /// Create a new `PageAlloc`.
    pub fn new(page_size: usize, target_overhead: usize) -> Self {
        let mut res = PageAlloc {
            target_overhead: target_overhead,
            creek: C::new(page_size),
            clean: SlagPipe::new_size(2),
            dirty: SlagPipe::new_size(8),
            _marker: PhantomData,
        };
        res.refresh_pages();
        res
    }

    /// Get more clean pages from the backing memory.
    fn refresh_pages(&mut self) {
        let creek = &self.creek;
        let iter = (0..4).map(|_| creek.carve(1));
        self.clean.bulk_add(iter);
    }
}

impl<C: MemoryBlock, D: DirtyFn> CoarseAllocator for PageAlloc<C, D> {
    type Block = C;

    fn backing_memory(&self) -> &C {
        &self.creek
    }

    unsafe fn alloc(&mut self) -> *mut u8 {
        if let Ok(ptr) = self.dirty.try_pop_mut() {
            trace_event!(grabbed_dirty);
            return ptr;
        }
        loop {
            if let Ok(ptr) = self.clean.try_pop_mut() {
                trace_event!(grabbed_clean);
                D::dirty(ptr);
                return ptr;
            }
            self.refresh_pages();
        }
    }

    unsafe fn free(&mut self, ptr: *mut u8, decommit: bool) {
        const MINOR_PAGE_SIZE: isize = 4096;
        use self::mmap::uncommit;
        use std::cmp;
        if decommit || self.dirty.size_guess() >= self.target_overhead as isize {
            let uncommit_len = cmp::max(0,
                                        self.backing_memory().page_size() as isize -
                                        MINOR_PAGE_SIZE) as usize;
            if uncommit_len == 0 {
                self.dirty.push_mut(ptr);
            } else {
                uncommit(ptr.offset(MINOR_PAGE_SIZE), uncommit_len);
                self.dirty.push_mut(ptr);
            }
        } else {
            self.dirty.push_mut(ptr);
        }
    }
}

/// A thread-local stack data-structure for caching allocations from an owned `Slag`.
///
/// This implementation is specialized in a few ways:
///
/// * While `mem` has a maximum size, it is assumed that all `push` calls will leave the
///   stack in a valid state. This is because the stack is initialized to have sufficient space
///   for an entire `Slag` of pointers for this particular size class, and all `push`es only
///   happen after the item in question has been confirmed to belong to a particular `Slag`.
///   Additional checking must be implemented on top of `PtrStack` if these are not the desired
///   semantics. See the `MagazineCache` implementation for an example of this.
///
/// * The stack is backed by an `OwnedArray`, which manages its own memory through `mmap`. This has
///   a number of advantages.  It reduces reliance on `Vec`-like structures that are tied to the
///   underlying `malloc` implementation. It also gives us lazy initialization without any extra
///   work. Fresh  will be uncommited: this means that potentially large allocations of memory for
///   stacks will only consume physical space when they are used.
struct PtrStack {
    data: OwnedArray<*mut u8>,
    top: usize,
}

impl PtrStack {
    fn new(max_objects: usize) -> PtrStack {
        PtrStack {
            data: OwnedArray::new(max_objects),
            top: 0,
        }
    }

    unsafe fn push(&mut self, item: *mut u8) {
        *self.data.get(self.top) = item;
        self.top += 1;
    }

    unsafe fn pop(&mut self) -> Option<*mut u8> {
        if self.empty() {
            None
        } else {
            self.top -= 1;
            Some(*self.data.get(self.top))
        }
    }

    #[inline]
    fn empty(&self) -> bool {
        self.top == 0
    }
}

/// A builder-pattern-style builder for `MagazineAllocator`s and `LocalAllocator`s.
///
/// ```rust,ignore
/// // A usize-specific allocator using the `LocalCache` frontend, customized to use 32K pages.
/// let la: LocalAllocator<usize> = AllocBuilder::default().page_size(32 << 10).build_local();
/// ```
///
/// Modifying other builder parameters past the default is not recommended. The overall API is
/// unstable.
pub struct AllocBuilder<T> {
    cutoff_factor: f64,
    page_size: usize,
    target_overhead: usize,
    eager_decommit_threshold: usize,
    max_objects: usize,
    _marker: PhantomData<T>,
}

impl<T> Default for AllocBuilder<T> {
    fn default() -> Self {
        AllocBuilder {
            cutoff_factor: 0.6,
            page_size: cmp::max(32 << 10, mem::size_of::<T>() * 4),
            target_overhead: 1 << 20,
            eager_decommit_threshold: 128 << 10,
            max_objects: 1 << 30,
            _marker: PhantomData,
        }
    }
}

impl<T> AllocBuilder<T> {
    pub fn cutoff_factor(&mut self, cutoff_factor: f64) -> &mut Self {
        self.cutoff_factor = cutoff_factor;
        self
    }
    pub fn page_size(&mut self, page_size: usize) -> &mut Self {
        self.page_size = page_size;
        self
    }
    pub fn target_overhead(&mut self, target_overhead: usize) -> &mut Self {
        self.target_overhead = target_overhead;
        self
    }
    pub fn eager_decommit_threshold(&mut self, eager_decommit_threshold: usize) -> &mut Self {
        self.eager_decommit_threshold = eager_decommit_threshold;
        self
    }
    pub fn max_objects(&mut self, max_objects: usize) -> &mut Self {
        self.max_objects = max_objects;
        self
    }

    /// Build a `LocalAllocator<T>` from the current configuration.
    pub fn build_local(&self) -> LocalAllocator<T> {
        LocalAllocator::new_standalone(self.cutoff_factor,
                                       self.page_size,
                                       self.target_overhead,
                                       self.eager_decommit_threshold,
                                       self.max_objects)
    }

    /// Build a `MagazineAllocator<T>` from the current configuration.
    pub fn build_magazine(&self) -> MagazineAllocator<T> {
        MagazineAllocator::new_standalone(self.cutoff_factor,
                                          self.page_size,
                                          self.target_overhead,
                                          self.eager_decommit_threshold,
                                          self.max_objects)
    }
}

macro_rules! typed_wrapper {
    ($name:ident, $wrapped:tt) => {
        pub struct $name<T>($wrapped<PageAlloc<Creek>>, PhantomData<T>);
        impl<T> Clone for $name<T> {
            fn clone(&self) -> Self {
                $name(self.0.clone(), PhantomData)
            }
        }

        impl<T> $name<T> {
            pub fn new_standalone(cutoff_factor: f64,
                                  page_size: usize,
                                  target_overhead: usize,
                                  eager_decommit: usize,
                                  max_objects: usize)
                -> Self {
                    let pa = PageAlloc::new(page_size, target_overhead);
                    let slag = SlagAllocator::new(max_objects, mem::size_of::<T>(), 0,
                                                  cutoff_factor, eager_decommit, pa);
                    $name($wrapped::new(slag), PhantomData)
                }

            pub unsafe fn alloc(&mut self) -> *mut T {
                self.0.alloc() as *mut T
            }

            pub unsafe fn free(&mut self, item: *mut T) {
                self.0.free(item as *mut u8)
            }
        }
        unsafe impl<T> Send for $name<T> {}
    };
}

typed_wrapper!(LocalAllocator, LocalCache);
typed_wrapper!(MagazineAllocator, MagazineCache);

/// Allocator state wrapping a `Slag`.
///
/// This struct forms the "backend" for a particular thread-local cache. It handles the state
/// transitions of different `Slag`s and also acquires new `Slag`s for iteration over the bitset.
pub struct SlagAllocator<CA: CoarseAllocator> {
    m: *mut Metadata,
    /// The current (local) `Slag`.
    slag: *mut Slag,
    /// Global pages, potentially not initialized to match `m`
    pages: CA,
    /// Available `Slag`s with metadata matching `m`.
    available: RevocablePipe<Slag>,
    /// Uncommit memory for full `Slag`s whose real memory footprint exceeds this threshold.
    eager_decommit_threshold: usize,
}

impl<CA: CoarseAllocator> Drop for SlagAllocator<CA> {
    fn drop(&mut self) {
        unsafe {
            let slag = self.slag;
            let meta = &*self.m;
            let (claimed, was) = (*slag).rc.unclaim();
            if claimed {
                // we used this slag at some point
                if was == meta.n_objects {
                    self.pages.free(slag as *mut u8, false);
                    trace_event!(transition_full);
                    // self.transition_full(slag, meta)
                } else if was >= meta.cutoff_objects {
                    self.transition_available(slag)
                }
            } else {
                // we never allocated from this slag, so just free it back to the page allocator
                self.pages.free(slag as *mut u8, false);
            }
        }
    }
}

unsafe impl<C: CoarseAllocator + Send> Send for SlagAllocator<C> {}

impl<CA: CoarseAllocator> SlagAllocator<CA> {
    pub fn partial_new(meta: *mut Metadata,
                       decommit: usize,
                       mut pa: CA,
                       avail: RevocablePipe<Slag>)
                       -> Self {
        let first_slag = unsafe { pa.alloc() } as *mut Slag;
        unsafe {
            Slag::init(first_slag, meta.as_ref().unwrap());
        };
        SlagAllocator {
            m: meta,
            slag: first_slag,
            pages: pa,
            available: avail,
            eager_decommit_threshold: decommit,
        }
    }
    pub fn new(max_objects: usize,
               object_size: usize,
               index: usize,
               cutoff_factor: f64,
               eager_decommit: usize,
               mut pa: CA)
               -> Self {
        // This is a bit wasteful as one metadata object consumes will wind up consuming a page. In
        // the dynamic allocator these are packed more tightly.
        let meta = Box::into_raw(Box::new(compute_metadata(object_size,
                                                           pa.backing_memory().page_size(),
                                                           index,
                                                           cutoff_factor,
                                                           max_objects)));
        let first_slag = unsafe { pa.alloc() } as *mut Slag;
        unsafe {
            Slag::init(first_slag, meta.as_ref().unwrap());
        };
        SlagAllocator {
            m: meta,
            slag: first_slag,
            pages: pa,
            available: RevocablePipe::new(),
            eager_decommit_threshold: eager_decommit,
        }
    }

    /// Re-initialize a non-empty `AllocIter`; potentially getting a new `Slag` to do so.
    unsafe fn refresh(&mut self) -> AllocIter {
        let s_ref = &*self.slag;
        let meta = &*self.m;
        let (_claimed, was) = s_ref.rc.unclaim();
        // We used to have this debug_assert
        //
        // debug_assert!(_claimed, "unclaiming slag on refresh");
        //
        // Why remove it? Because we now call "refresh" on the initial allocation performed
        // up-stream by a cache data-structure. These initial slags will be unclaimed (hence
        // hitting the first branch of the if below). The comment above that branch does not apply
        // to the initialization case.
        debug_assert_eq!(*meta, *(*s_ref).meta.load(Ordering::Relaxed));

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
            debug_assert!(_claimed,
                          "claiming slag either during initialization or due to being over cutoff");
            s_ref.refresh(meta)
        } else {
            // we need a new slag!
            // first we try and get a slag from the available slagpipe. If it is empty, then we get
            // a fresh page from PageAlloc and initialize it with the current object class's
            // metadata.
            let next_slab = match self.available.try_pop_mut() {
                Ok(slab) => {
                    trace_event!(grabbed_available);
                    slab
                }
                Err(_) => {
                    let new_raw = self.pages.alloc() as *mut Slag;
                    if (*new_raw).meta.load(Ordering::Relaxed) != self.m {
                        Slag::init(new_raw, meta);
                    }
                    new_raw
                }
            };
            self.slag = next_slab;
            let s_ref = self.slag.as_mut().expect("s_ref_2"); // let s_ref = &*self.slag;
            let claimed = s_ref.rc.claim();
            debug_assert!(claimed, "claiming new slag after refresh");
            s_ref.refresh(meta)
        }
    }

    fn transition_available(&mut self, slag: *mut Slag) {
        trace_event!(transition_available);
        self.available.push_mut(slag)
    }

    #[cfg_attr(feature = "cargo-clippy", allow(inline_always))]
    #[inline(always)]
    unsafe fn transition_full(&mut self, slag: *mut Slag, meta: &Metadata) {
        let real_size = meta.usable_size;
        if RevocablePipe::revoke(&slag) {
            trace_event!(transition_full);
            self.pages
                .free(slag as *mut u8, real_size >= self.eager_decommit_threshold)
        }
        // Otherwise caught in a strange race condition (see comments in alloc). We can
        // safely return without further work.
    }

    unsafe fn bulk_free(&mut self,
                        mask: usize,
                        word: *mut Word,
                        slag: *mut Slag,
                        meta: &Metadata) {
        let n_ones = mask.count_ones() as usize;
        if n_ones == 0 {
            return;
        }
        trace_event!(bulk_remote_free);
        let s_ref = &*slag;
        let (claimed, was) = s_ref.rc.inc_n(n_ones);
        let before = (*word).fetch_or(mask, Ordering::Release);
        debug_assert_eq!(before & mask,
                         0,
                         "Invalid mask: transitioned\n{:064b} with \n{:064b}",
                         before,
                         mask);
        let now = was + n_ones;
        if !claimed {
            if now == meta.n_objects {
                self.transition_full(slag, meta);
            } else if was < meta.cutoff_objects && now >= meta.cutoff_objects {
                self.transition_available(slag);
            }
        }
    }

    /// Perform a "remote" free to the `Slag` containing `item`.
    unsafe fn free(&mut self, item: *mut u8) {
        trace_event!(remote_free);
        let meta = &*self.m;
        let it_slag = Slag::find(item, meta.total_bytes);
        match it_slag.as_ref().unwrap().free(item) {
            Transition::Null => return,
            Transition::Available => self.transition_available(it_slag),
            Transition::Full => self.transition_full(it_slag, meta),
        }
    }

    /// Test if `it` is an element of the current `Slag`.
    fn contains(&self, it: *mut u8) -> bool {
        unsafe {
            let meta = self.m.as_ref().unwrap();
            let it_slag = Slag::find(it, meta.total_bytes);
            it_slag == self.slag
        }
    }
}

impl<CA: CoarseAllocator> Clone for SlagAllocator<CA> {
    fn clone(&self) -> Self {
        let mut new_page_handle = self.pages.clone();
        let first_slag = unsafe { new_page_handle.alloc() as *mut Slag };
        unsafe {
            Slag::init(first_slag, self.m.as_ref().unwrap());
        };
        SlagAllocator {
            m: self.m,
            slag: first_slag,
            pages: new_page_handle,
            available: self.available.clone(),
            eager_decommit_threshold: self.eager_decommit_threshold,
        }
    }
}



#[cfg(test)]
mod tests {
    extern crate env_logger;
    use super::*;
    use std::thread;
    use std::ptr::write_volatile;
    use std::collections::HashSet;

    #[test]
    fn metadata_basic() {
        let _ = env_logger::init();
        compute_metadata(8, 4096, 0, 0.8, 4);
        compute_metadata(16, 4096, 0, 0.8, 1024);
        compute_metadata(24, 4096, 0, 0.8, 1024);
        compute_metadata(127, 4096, 0, 0.8, 1024);
        compute_metadata(800, 2 << 20, 0, 0.8, 32 << 10);
        compute_metadata(514, 4096, 0, 0.8, 1024);
        compute_metadata(513, 2 << 20, 0, 0.8, 1024);
        compute_metadata(768, 4096, 0, 0.8, 1024);
        compute_metadata(800, 4096, 0, 0.8, 32 << 10);
        compute_metadata(1025, 4096, 0, 0.8, 32 << 10);
    }

    #[test]
    fn obj_alloc_basic() {
        let _ = env_logger::init();
        let mut oa = AllocBuilder::<usize>::default().page_size(4096).build_local();
        unsafe {
            let item = oa.alloc();
            write_volatile(item, 10);
            oa.free(item);
        }
    }

    #[test]
    fn obj_alloc_many_pages_single_threaded_usize() {
        obj_alloc_many_pages_single_threaded::<usize>();
    }

    #[test]
    fn obj_alloc_many_pages_single_threaded_u24() {
        obj_alloc_many_pages_single_threaded::<[u8; 24]>();
    }

    #[test]
    fn obj_alloc_many_pages_single_threaded_u32() {
        obj_alloc_many_pages_single_threaded::<[u8; 32]>();
    }

    fn obj_alloc_many_pages_single_threaded<T: 'static>() {
        let _ = env_logger::init();
        const N_ITEMS: usize = 4096 * 20;
        let mut oa = AllocBuilder::<T>::default().page_size(4096).build_local();
        assert!(mem::size_of::<T>() >= mem::size_of::<usize>());
        // stay in a local cache
        for _ in 0..N_ITEMS {
            unsafe {
                let item = oa.alloc();
                write_volatile(item as *mut usize, 10);
                oa.free(item);
            }
        }

        let mut v = Vec::with_capacity(N_ITEMS);
        let mut h = HashSet::new();
        for i in 0..N_ITEMS {
            unsafe {
                let item = oa.alloc();
                write_volatile(item as *mut usize, i + 1);
                v.push(item);
                let item_num = item as usize;
                assert!(!h.contains(&item_num));
                h.insert(item_num);
            }
        }

        for i in v {
            unsafe {
                oa.free(i);
            }
        }
    }
    #[test]
    fn obj_alloc_many_pages_many_threads_usize() {
        obj_alloc_many_pages_many_threads::<usize>()
    }

    #[test]
    fn obj_alloc_many_pages_many_threads_u24() {
        obj_alloc_many_pages_many_threads::<[u8; 24]>()
    }

    #[test]
    fn obj_alloc_many_pages_many_threads_u32() {
        obj_alloc_many_pages_many_threads::<[u8; 32]>()
    }

    fn obj_alloc_many_pages_many_threads<T: 'static>() {
        let _ = env_logger::init();
        use std::mem;
        const N_ITEMS: usize = 4096 * 4;
        const N_THREADS: usize = 40;
        // TODO make macros for these tests and test both MagazineAllocator and LocalAllocator
        let oa = AllocBuilder::<T>::default().page_size(4096).build_magazine();
        // stay in a local cache
        assert!(mem::size_of::<T>() >= mem::size_of::<usize>());
        let mut threads = Vec::new();
        for _ in 0..N_THREADS {
            let mut my_alloc = oa.clone();
            threads.push(thread::spawn(move || {
                for _ in 0..N_ITEMS {
                    unsafe {
                        let item = my_alloc.alloc();
                        write_volatile(item as *mut usize, 10);
                        my_alloc.free(item);
                    }
                }

                let mut v = Vec::with_capacity(N_ITEMS);
                let mut h = HashSet::new();
                for i in 0..N_ITEMS {
                    unsafe {
                        let item = my_alloc.alloc();
                        write_volatile(item as *mut usize, i);
                        v.push(item);
                        let item_num = item as usize;
                        assert!(!h.contains(&item_num));
                        h.insert(item_num);
                    }
                }

                for i in v {
                    unsafe {
                        my_alloc.free(i);
                    }
                }
            }));
        }
        for t in threads {
            t.join().expect("threads should exit successfully");
        }
    }

}
