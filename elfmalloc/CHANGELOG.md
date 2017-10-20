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
- Implemented `Alloc` trait
- Implemented `malloc-bind`'s `LayoutFinder` and `Malloc` traits
- Added new metadata management mechanism that eliminates dependency on the
  `Creek` and overcommit more generally
- Substantial refactoring of the code into separate traits/modules
- Added experimental magazine/depot-style frontend. Currently still
  experimental; it is almost always slower than `LocalCache` and
  `MagazineCache`
- Added `c-api` feature to optimize for the C malloc API

### Fixed
- Fixed a bug preventing non-nightly builds from compiling
- Fixed an integer multiplication overflow bug
- Added workaround to avoid double-drop behavior in certain `malloc` workloads
- Fixed "recursive `malloc`" bug caused by failing to initialize the `crossbeam`
  TLS early enough
- Fixed recursive allocation bugs during printing and asserting by switching to
  the macros from the `alloc-fmt` crate
