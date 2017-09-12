<!-- Copyright 2017 the authors. See the 'Copyright and license' section of the
README.md file at the top-level directory of this repository.

Licensed under the Apache License, Version 2.0 (the LICENSE-APACHE file) or
the MIT license (the LICENSE-MIT file) at your option. This file may not be
copied, modified, or distributed except according to those terms. -->

# Allocators, general and specific

This repo includes a number of custom dynamic memory allocators written in Rust,
along with some utilities used to implement these allocators. The `info`
directory contains more detailed information about specific components of the
project (e.g. performance measurements). Aside from `info`, all top-level
directories are Rust crates. More detailed information on each crate can be
found in each top-level directory's readme.

## Allocator Crates

* `slab-alloc` includes an implementation of a slab allocator in the same
  tradition as the original [slab allocator](https://www.usenix.org/legacy/publications/library/proceedings/bos94/full_papers/bonwick.a)
  by Jeff Bonwick (though several details are changed from the original). Some
  key points to consider here are:

  * Slab allocators are object-specific: they can only create objects of a
    specified size and alignment.

  * These slab allocators are currently limited to use by a single thread.

  * Slabs can be used in a way that tracks whether freed objects have already
    been initialized. This allows expensive constructor calls to be elided under
    some circumstances.

* `elfmalloc` includes a general multi-threaded allocator. `elfmalloc` is fully
   thread-safe and can be used as a drop-in replacement for `malloc`. See the
   `elfc` crate for how to go about doing this. This general allocator is
   implemented as an ensemble of size-specific allocators. As a result,
   size-specific allocators are also exposed for that more specialized
   use-case. These object-specific allocators do not currently track
   initialization, but they scale well in a multi-threaded setting, giving them
   a complementary set of use-cases when compared to `slab-alloc`.

* `bsalloc` is a very simple dynamic memory allocator used to bootstrap
  `elfmalloc` in the cases when it depends on libraries that allocate memory on
  the heap. Using `bsalloc` allows our code to be completely self-hosting (not
  relying on whatever `malloc` Rust would otherwise use), without having to
  re-write key library infrastructure like `crossbeam` that relies on a dynamic
  memory allocation.

* `mmap-alloc` is an allocator that constructs anonymous virtual memory maps.
  There are enough edge-cases and cross-platform issues with calling `mmap`
  directly, that it made sense to make this its own crate.

## Utility Crates

* `bagpipe` provides a number of key concurrent queue-like data-structures. In
  addition to implementing a number of state-of-the-art concurrent MPMC queues
  in the `crossbeam` framework, it also exposes a new data-structure: the
  `Bagpipe`. `Bagpipes` are like queues, but they trade consistency for
  scalability. This is normally a dubious exchange to make, but for
  `elfmalloc`, `Bagpipe`s are used as "free lists" where ordering on the
  insertion and removal of elements is not required.

* `object-alloc` includes a number of traits currently used to define
  object-specific allocators. Its traits are currently only implemented by the
  allocators in `slab-alloc`, though we intend this to change over time.

* `object-alloc-test` provides a number of tests that check for correctness of
  an arbitrary object allocator (i.e. an allocator implementing one of the
  traits in `object-alloc`).

* `malloc-bind` is a crate that provides macros to transform any general
  allocator that can store its own size and alignment-related metadata into a
  full `malloc` implementation. The intent is to replace `elfc` with this more
  generic solution in the future.

## Contributing

Interested in contributing? We'd love to have you! Check out [CONTRIBUTING.md](https://github.com/ezrosent/allocators-rs/blob/master/CONTRIBUTING.md)

## Copyright and license

Copyrights in this project are retained by their contributors. No copyright
assignment is required to contribute to this project. For a list of authors, see
this repository's version control history.

This project is dual-licensed under the  [Apache
2.0](http://www.apache.org/licenses/LICENSE-2.0) license or the
[MIT](http://opensource.org/licenses/MIT) license at your option. Copies of
these licenses can be found in the `LICENSE-APACHE` and `LICENSE-MIT` files
respectively.
