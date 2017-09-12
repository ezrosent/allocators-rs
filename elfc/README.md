<!-- Copyright 2017 the authors. See the 'Copyright and license' section of the
README.md file at the top-level directory of this repository.

Licensed under the Apache License, Version 2.0 (the LICENSE-APACHE file) or
the MIT license (the LICENSE-MIT file) at your option. This file may not be
copied, modified, or distributed except according to those terms. -->

This stubs out the dynamic allocators in this repo for use as a Linux-like
`malloc` implementation via `LD_PRELOAD`. Make sure to compile with
`--features=nightly`, performance suffers drastically without this feature
enabled due to some thread-local storage issues. Once compiled, the resulting
`libelfc.so` can be used in `LD_PRELOAD` to route `malloc` calls through
`elfmalloc`. Note that OSX and Windows are currently unsupported, though we
plan to add these in the future.

NB: `elfmalloc` requires a large virtual memory-mapped region (ideally over 1
TB); enabling overcommit in Linux may be required for things to function
properly.

This will be replaced with `malloc_bind` soon.
