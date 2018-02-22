<!-- Copyright 2017-2018 the authors. See the 'Copyright and license' section of the
README.md file at the top-level directory of this repository.

Licensed under the Apache License, Version 2.0 (the LICENSE-APACHE file) or
the MIT license (the LICENSE-MIT file) at your option. This file may not be
copied, modified, or distributed except according to those terms. -->

# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/).

## [Unreleased]

### Added
- Added support for committing on Linux and Mac
- Added support for configuring commit on allocation on Mac
- Added support for alignments larger than a memory page (`large-align`
  feature)

### Changed
- Upgraded to new `UntypedObjectAlloc` trait that uses `NonNull<u8>` instead
  of `*mut u8`
- Improved documentation on committed vs. uncommitted memory

## 0.2.0

### Added
- Added this changelog
- Added `commit` and `uncommit` methods on Windows
- Added the ability to configure whether `alloc` commits memory
- Added documentation about instruction cache incoherency
- Added support for full Alloc API (`shrink_in_place`, `grow_in_place`,
  `realloc`)
    - In Linux, these functions can use `mremap` to grow/shrink beyond the size
      of a page
- Added tests for memory permissions on Linux (by parsing `/proc/<pid>/maps`)
  and Windows (by using the `VirtualQuery` function)

### Removed
- Removed huge page support
- Removed `commit` method on on Linux and Mac
- Removed tests for `mmap`s at NULL on Linux and Mac, as it turns out they are
  guaranteed not to happen
- Removed `test-no-std` feature

### Changed
- Changed the way boolean configuration methods on `MapAllocBuilder` work
  (previously, for each configuration option, there was a pair of methods to
  enable and disable that option; now, there's a single method that accepts a
  boolean indicating whether the option should be enabled)

### Fixed
- Fixed a bug that prevented compilation on 32-bit Windows
- Fixed a bug caused by `sysconf` 0.3.0 that prevented compilation on Windows
  by upgrading to 0.3.1
- Fixed a bug that failed to round allocations up correctly to the next multiple
  of the page size
