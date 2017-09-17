// Copyright 2017 the authors. See the 'Copyright and license' section of the
// README.md file at the top-level directory of this repository.
//
// Licensed under the Apache License, Version 2.0 (the LICENSE-APACHE file) or
// the MIT license (the LICENSE-MIT file) at your option. This file may not be
// copied, modified, or distributed except according to those terms.

//! Global tags to indicate the allocation subsystem from which a pointer originates.
//!
//! The `AllocType` enum is used to mark pages in elfmalloc; see the `general` and `slag` modules
//! for more on how this works.

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum AllocType {
    SmallSlag,
    BigSlag,
    Large,
}
