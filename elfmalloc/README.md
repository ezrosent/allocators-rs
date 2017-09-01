<!-- Copyright 2017 the authors. See the 'Copyright and license' section of the README.md file at the top-level directory of this repository.

Licensed under the Apache License, Version 2.0 (the LICENSE file). This file may not be copied, modified, or distributed except according to those terms. -->

# Efficient Dynamic Memory Allocation

This crate provides efficient multi-threaded heap allocation both on a
per-object (i.e. fixed-size) or dynamic (i.e. `malloc`-like) basis.
Most of the details are currently provided in the crate documentation.

Note that the allocators in this crate only work on 64-bit machines
right now. There are currently some ideas on how to add 32-bit support,
but any such changes would require serious additions to the allocators'
designs.
