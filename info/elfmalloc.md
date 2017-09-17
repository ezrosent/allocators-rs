<!-- Copyright 2017 the authors. See the 'Copyright and license' section of the
README.md file at the top-level directory of this repository.

Licensed under the Apache License, Version 2.0 (the LICENSE-APACHE file) or
the MIT license (the LICENSE-MIT file) at your option. This file may not be
copied, modified, or distributed except according to those terms. -->

# `elfmalloc` Design

This document provides a brief overview of the design of `elfmalloc`. It
currently leaves out a lot of the gory details. While that may change in the
future, the current best source of information on those is the code itself and
the documentation that accompanies it. The intent is that this summary will
equip the reader with enough context that the code's documentation will be more
accessible. We currently do not yet have a full related-work section here, as
`malloc` has a long history. Until then, we briefly mention that this design is
indebted to various other memory allocators:

* Solaris slab allocators, in their initial and magazine-based forms.
* "Caching allocators" like TCMalloc, Hoard, and jemalloc.
* Scalloc: an allocator emphasizing global concurrent data-structures.

## Global Data-Structures

Elfmalloc is built out of a few key abstractions. We begin with the
lower-level aspects of memory management.

### Memory Sources

Elfmalloc is parameterized on a low-level interface for acquiring memory
from the system (e.g. Linux). This interface is called a `MemorySource`:

```rust
trait MemorySource where Self: Clone {
    fn new(page_size: usize) -> Self;
    /// The smallest unit of memory that can be `carve`d.
    fn page_size(&self) -> usize;
    /// Return `npages` fresh pages from the `Creek`, aligned to the
    /// current source's page size.
    fn carve(&self, npages: usize) -> Option<*mut u8>;
}
```

Note that the source's "page size" is permitted to be larger than the underlying
operating system's page size (4KiB on Linux). A `MemorySource` implementation
may have to do additional work to ensure that this higher alignment is
guaranteed.

So what implements `MemorySource`? The simplest example is the `MmapSource`,
which simply calls `mmap` under the hood, allocating sufficient slack space to
ensure some sub-span of the returned memory is aligned to the page size.

#### The Creek

An older version of elfmalloc relied on a `MemorySource` that could also test
*ownership*: one that also has a `contains` method for testing whether or not a
pointer was allocated from that source. Doing this efficiently with the
`MmapSource` is tough because there is no way to enforce that all pages returned
from the source are returned from a contiguous chunk of memory. Without that
guarantee,  some form of global bookkeeping is required to keep track of which
regions were and were not returned from the source.

We used a different solution to implement this interface: the Creek.

At initialization time, a creek maps a large (i.e. many terabytes) region of
memory.  It is divided into fixed-size pages. In `elfmalloc` these pages are 2
megabytes by default, though smaller object classes only use a small fraction of
this.

All of this scaffolding relies on the fact that the virtual address space is
quite large on 64-bit machines, and that memory-mapped memory can be lazily
initialized.

We required he `contains` method to differentiate pages from the
`Creek` from ones that correspond to objects larger than the page size, which
are `mmap`-ed directly. For the `Creek`, this is just a membership check using
the base and maximum address of the `Creek`. We have since found a way around
such an ownership check (see below).

### The `PageAlloc`

The `PageAlloc` is two `Bagpipe`s of pages taken from the `Creek`. The first
`Bagpipe` contains clean pages that have not been written to. The second
`Bagpipe` contains pages that may be dirty. As its name suggests `PageAlloc` has
`alloc` and `free` methods that operate on page-sized chunks of memory.
Allocations start by trying to get a dirty page, if that fails they attempt a
clean page and finally they fall back to the `Creek`. If an allocation gets that
far is refills the clean `Bagpipe` with several pages. Deallocations, of course,
push to the dirty `Bagpipe`.

*Room for Growth* We currently do not have any algorithms for reclaiming dirty
pages (i.e.  uncommit the memory with `madvise` and move it to the clean
`Bagpipe`). Earlier attempts at setting a hard limit on these pages led to
degraded performance under certain circumstances. The best option is probably
something akin to the working set algorithm currently in use in the `slab_alloc`
crate.

## Slabs

The slab data-structure is what allows us to allocate multiple relatively small
objects out of a single page.

### Layout

They have the following layout (where explicit mentions of 64 or 63 should be
understood to be stand-ins for the machine word size):

```
 --------------------------------------------------------------------------------------------------------------------------------
| header (256 bits) | (padding bits) | bit-set (ceil(bits per object * n_objects / 64) bits) | objects (n_objects * object_size) |
 --------------------------------------------------------------------------------------------------------------------------------
```

The header itself has the following layout:

```
 -------------------------------------------------------------------------------------------------------------------------------
| alloc type (64 bits) | claimed (1 bit) | reference count (63 bits) | Metadata pointer (64 bits) | Revocation handle (64 bits) |
 -------------------------------------------------------------------------------------------------------------------------------
```

We explain each of these in turn.

**The alloc type** is used to distinguish between different allocation
subsystems in elfmalloc. This part of the algorithm is explained in greater
detail later in this document.

**The claimed bit** indicates if the slab is currently owned by a thread. Slabs
that are owned by a particular thread cannot be inserted or removed from any of
the allocator's global data-structures. A given thread can own one slab at a
time: it uses that slab to service allocations.

**The reference count** indicates how many objects are available within the
slab. @joshlf has pointed out that these semantics mean this is not a reference
count *per se*; when this reference count is 0 it means that the maximum number
of references to this slab are held! An important detail here is that the
claimed bit and the reference count are held in the same machine word. As a
result, they can be updated and read together atomically using a single
instruction.

**The metadata pointer** contains a ... pointer to slab metadata! Metadata
describes a host of important slab attributes, including the free parameters in
the slab layout diagram:

  * The number of objects in a slab.
  * The size of the objects stored in the slab.
  * The padding (used for alignment) stored between the header and the start of
    the bit-set.
  * The number of bits used to represent a single object in the bit-set.

Metadata also includes information about the total usable size of a slab. For
smaller object sizes, the usable size is much smaller than a total page. This
increases the our ability to re-use slabs, and it comes at little memory cost
because the rest of the page is simply uncommitted. This is directly analogous
to `scalloc`'s distinction between *virtual* and *real* spans.

**The revocation handle** is an opaque `AtomicUsize` used to remove a slab from
an intermediate bagpipe. See the `Bagpipe` overview for more information on this
feature.

**Padding bits** are used to ensure that power-of-2 object sizes are aligned to
their size.

**The bit-set** is an array of bits. In most cases, the *i*th bit indicates if
the *i*th object is available to allocate. Sometimes this mapping from bit to
object is a little more complicated. In order to avoid dividing by a number that
is not a power of two, we occasionally "pretend" an object is actually an array
of smaller power-of-2-sized objects. In such cases, the "bits per object" value
is greater than 1, and set bits simply point to the beginning of an object in
the array.

**The objects** are simply an array of objects: the objects stored in this
particular slab.

### Slab life-cycle

1. *Uninitialized* Slabs move through various states over the course of
   their use. Before they are even initialized, slabs live in the page allocator
   (described above) as either clean or dirty pages. If a thread needs to
   service an allocation for a particular size, and no slabs can be re-used it
   allocates a page from the page allocator.

2. *Claimed* Once a thread pops a slab from one of the available `Bagpipe`s, it
   sets its claimed bit. After that it services allocations by iterating over
   the bit-set, potentially aided by additional front-end logic. If a slab has
   been exhausted, the claimed bit is set to 0 and the thread acquires a new
   slab. Such slabs are called *floating* because they are neither owned by a
   particular thread, nor are they present in any global data-structures.

3. *Floating* slabs can only be accessed by deallocations from that slab. These
   deallocations are by nature *remote* frees, and a thread rounds down to the
   nearest page to access the required slab. These deallocations work by setting
   the corresponding bit in the bit-set corresponding to the object being
   returned, and incrementing the reference count. Each of these steps can be
   performed by a single atomic instruction (`fetch-or` and `fetch-add`) neither
   of which need to be retried.

   If the result of the `fetch-add` operation shows both an unset claimed bit
   *and* a value equal to the *reuse threshold* (a value in the slab metadta),
   then the slab is pushed to the *available* `Bagpipe`.

4. *Available* slabs are those that are present in this *available* `Bagpipe`.
   This structure contains slabs with the same metadata (i.e.  the same object
   size class) that have had sufficient objects freed to them that they can be
   claimed once again. Once in this state, a slab can be transitioned to either
   claimed or uninitialized. The former transition occurs when an allocating
   thread pops it from the `Bagpipe`. The latter transition occurs when a remote
   free increments the reference count to the maximum objects available in the
   slab. In this case, the slab is `revoke`d from the available `Bagpipe` and
   freed back to the page allocator. From there, it can be reused by any size
   class.

Many details were skipped in this overview, mainly those dealing with how these
state transitions can be performed safely (e.g. as stated above, there is a race
condition between threads transitioning 3 -> 4 and 4 -> 1). To get those
details, check out the code in `elfmalloc/src/slag.rs`. If something that should
be commented is not, file an issue!

**Warning**: In the code, slabs are called `Slag`s. This is a portmanteau of
the terms *slab* and *magazine*, the latter term originating from Bonwick and
Adams'
[paper](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.97.708&rep=rep1&type=pdf)
on the updated Solaris slab allocator. We originally used this term in order to avoid
confusion with the more traditional slabs of the `slab_alloc` crate. The main
downside is that the term `Slag` is quite ugly, while "slab" is both less
grating and more standard.

## Frontends

This is where the rubber meets the road. The slab-level allocate and free
operations both require atomic operations to perform: allocations require atomic
`swap` and `fetch-add` instructions every time a new word is started in the
bit-set, frees require an atomic `fetch-or` and `fetch-add`. While these
operations are quite fast given that they implement (to our knowledge)
thread-safe and correct protocols, they are no substitute for unsynchronized
thread-local operations.

As a result, we have a front-end that performs a small amount of caching in
thread-local allocation buffers. This algorithm is dead-simple: we have a
fixed-size stack per object class. Allocations first attempt to pop from this
stack, only going to the claimed slab if the stack is empty. Symmetrically,
frees first attempt to push onto this stack. If the stack is full, all of its
contents are freed remotely. This structure allows us to coalesce these remote
free operations (composing masks ahead of the `fetch-or`), reducing the number
of atomic operations required for a particular remote free operation.

The code is structured to support multiple different front-ends. We have an
experimental alternative that only caches local allocations in a local stack,
eagerly performing remote free operations. See `elfmalloc-performance.md` for
more information on the performance trade-offs of these two approaches.

## General Allocation

To support full generic allocation, we simply have an array of size-specific
allocators. Each size class has its own available `Bagpipe`; there are two
over-arching `PageAlloc`s that cache pages for small and medium objects.
Allocations that are larger than the maximum medium-sized object fall back on
`mmap` (see the `large_alloc` module in `general.rs`). Small objects and medium
objects have two different page sizes: this is to avoid holding megabytes of
memory hostage if a thread is only allocating a small number of 8-byte objects.

Check out `elfmalloc/src/general.rs` for more of the details on how we get this
to work. They are not particularly interesting from an algorithmic perspective,
but they may be helpful for someone trying to do something similar in Rust. We
conclude with more information about allocation type metadata.

### Determining Allocation Source During Deallocation

Between the small, medium and large objects  there are 3 different paths that a
call to `free` can take depending on on the object being deallocated.

1. For small objects, round down to the small objects' page size and read slab
   metadata for object size information, etc.
2. For medium objects, do the same operation as (1), but for the medium objects'
   page size.
3. For large objects, look up the size of the allocation in padding.

Of course, we only get a pointer when someone calls `free`. We have to figure
out which of these paths to take. Our solution is to stash metadata about which
path to take aligned to the medium objects' page size. Here is our metadata
type:

```rust
pub enum AllocType {
    // Small objects
    SmallSlag,
    // Medium objects
    BigSlag,
    // Large objects
    Large,
}
```

To illustrate, we will use a worked example: let the small page size be 128KiB
and the large page size be 2MiB, with `mmap` guaranteeing allocations aligned to
4KiB.

The algorithm for looking up the free path for a pointer `p: *mut u8` is to
round `p.offset(-1)` (`p-1` if this were C) down to the nearest 2MiB address. We
guarantee that this address holds a pointer to the object's corresponding
`AllocType`.


#### How do we ensure the types are present?

1. For small objects, we allocate pages in groups of 16 (1 medium-object page)
   using a 2MiB `MemorySource` to ensure the first of these pages is aligned to
   the proper size. We then set the beginning of this region to `SmallSlag`
   before adding the extra pages to the `PageAlloc`'s clean `BagPipe`. We
   declare the slab header as `repr(C)` to ensure that the slab's `AllocType`
   field is stored first. This prevents slab initialization from clobbering the
   alloc type field.

2. Medium objects are easy: the slab initialization process will automatically
   place an `AllocType` on the 2MiB boundary required by the protocol

3. For large objects, we simply allocate an additional 2MiB of space, return
   2MiB into the region from `malloc`, and round that address down to 2MiB to
   store type information. It is of course possible that the returned address
   will be 2MiB-aligned. In that case, the metadata is stored at the very start
   of the allocated region. This edge case is the reason why we take
   `offset(-1)` from the pointer before rounding down to the 2MiB boundary.
