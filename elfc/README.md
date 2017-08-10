This stubs out the dynamic allocators in this repo for use as a
Linux-like `malloc` implementation via `LD_PRELOAD`. Make sure to
compile with `--features=nightly`, performance suffers drastically
without this feature enabled due to some thread-local storage issues.

This will be replaced with `malloc_bind` soon.
