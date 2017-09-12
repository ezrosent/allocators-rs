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

### Changed
- Made `posix_memalign` only compile on Linux and Mac
- Made `cfree` only compile on Linux
- Made `valloc` only compile on Linux and Mac
- Made `LayoutFinder` and its methods unsafe

### Fixed
- Implemented missing logic to better match the semantics of the C allocation
  API
- Fixed a bug caused by `sysconf` 0.3.0 that prevented compilation on Windows
  by upgrading to 0.3.1
- Made it so that the correct lower bound on alignment is used on Windows
- Set `errno` in more places where it should have been set before
