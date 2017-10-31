<!-- Copyright 2017 the authors. See the 'Copyright and license' section of the
README.md file at the top-level directory of this repository.

Licensed under the Apache License, Version 2.0 (the LICENSE-APACHE file) or
the MIT license (the LICENSE-MIT file) at your option. This file may not be
copied, modified, or distributed except according to those terms. -->

# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/).

## [Unreleased]

### Added
- Added this changelog
- Added `_aligned_malloc` on Windows
- Added documentation on the behavior of each of the functions in the C
  allocation API

### Changed
- Put some `unlikely` in `if`s
- Made `cfree` only compile on Linux
- Made `posix_memalign` and `valloc` only compile on Linux and Mac
- Made `LayoutFinder` and its methods unsafe
- Made `Malloc` a trait rather than a struct, allowing methods to be overridden
  by implementors

### Removed
- Removed the `reallocarray` function, as it was spuriously marked as being a
  Linux extension (it is actually an OpenBSD extension, and we don't currently
  support OpenBSD)

### Fixed
- Fixed a bug caused by `sysconf` 0.3.0 that prevented compilation on Windows
  by upgrading to 0.3.1
- Made it so that the correct lower bound on alignment is used on Windows and
  Mac
- Set `errno` in places where it should have been set before
- Made it so that 0-sized allocations only return `NULL` on Linux (they
  previously returned `NULL` on Mac and Windows as well, which is incorrect)
- Check that `alignment` is a power of two in `aligned_alloc`
- Changed macros to use absolute paths to types to avoid possible issues with
  scoping and imports
