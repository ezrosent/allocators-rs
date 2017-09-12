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
- Added `commit` and `uncommit` methods on Windows
- Added the ability to configure whether `alloc` commits memory
- Added documentation about instruction cache incoherency

### Removed
- Removed `commit` method on on Linux and Mac

### Fixed
- Fixed a bug that prevented compilation on 32-bit Windows
- Fixed a bug caused by `sysconf` 0.3.0 that prevented compilation on Windows
  by upgrading to 0.3.1
- Fixed a bug that failed to round allocations up correctly to the next multiple
  of the page size
