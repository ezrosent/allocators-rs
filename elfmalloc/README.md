# Efficient Dynamic Memory Allocation

This crate provides efficient multi-threaded heap allocation both on a
per-object (i.e. fixed-size) or dynamic (i.e. `malloc`-like) basis.
Most of the details are currently provided in the crate documentation.

Note that the allocators in this crate only work on 64-bit machines
right now. There are currently some ideas on how to add 32-bit support,
but any such changes would require serious additions to the allocators'
designs.
