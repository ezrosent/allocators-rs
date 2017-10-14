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
- Added this changelog
- Bagpipes can now call drop on their elements when they are dropped. This does
  not happen automatically, but there is a new trait to inject cleanup callbacks
  to `BagPipe` shutdown.

### Changed
- Bagpipes now store a Crossbeam Epoch GC instance rather than using the global
  singleton instance so that using a Bagpipe does not rely on thread-local
  storage.

### Fixed
- Fixed a bug where crossbeam TLS would remain uninitialized upon cloning a
  `BagPipe`, resulting in stack overflow in `elfmalloc`.
