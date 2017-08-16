This stubs out the dynamic allocators in this repo for use as a Linux-like
`malloc` implementation via `LD_PRELOAD`. Make sure to compile with
`--features=nightly`, performance suffers drastically without this feature
enabled due to some thread-local storage issues. Once compiled, the resulting
`libelfc.so` can be used in `LD_PRELOAD` to route `malloc` calls through
`elfmalloc`. Note that OSX and Windows are currently unsupported, though we
plan to add these in the future.

This will be replaced with `malloc_bind` soon.
