//! This module includes some lightweight utilities for tracing different allocation events.
//!
//! It is currently very rough. Despite being entirely safe Rust, `trace` mode will sometimes cause
//! binaries to segfault when stats are enabled. This may have to do with calling printing-related
//! routines so early in tls initialization.
//!
//! Like all global abstractions in use here, some hacks are required to get this to exit cleanly.
//!
//! ## `THREAD_CTR`
//!
//! We use a global `AtomicUsize` to creat thread Ids rather than `thread::current().id()`. This is
//! because the latter method panics somewhere when setting thread metadata during thread creation.
//! This is presumably some ill-fated `malloc` dependency.
//!
//! ## `RefCell`
//!
//! While `RefCell` is probably the correct thing to use here regardless, we really must use some
//! version of it. This is because the stat collection infrastructure can recursively call itself
//! during thread initialization time because TLS can call `calloc`. This is similar to a problem
//! encountered in the `general::global` in this crate. `RefCell`'s `try_borrow` gives us a guard
//! against this recursion.
use std::cell::RefCell;
use std::sync::atomic::{AtomicUsize, Ordering};

type Num = i64;

#[derive(Default, Debug, Clone, Copy)]
/// Counters for various useful events. Primarily used for debugging and performance.
pub struct AllocStats {
    /// Allocations from the owned `Slag`
    pub slag_alloc: Num,
    /// Allocations from a local cache
    pub cache_alloc: Num,

    /// Frees to a local cache.
    pub local_free: Num,
    /// Frees to a remote `Slag`
    pub remote_free: Num,
    /// Bulk frees to a remote `Slag`
    pub bulk_remote_free: Num,

    /// Transition a floating `Slag` to the available state
    pub transition_available: Num,
    /// Successfully transition an available `Slag` to the full state.
    pub transition_full: Num,

    /// Acquire an available `Slag`
    pub grabbed_available: Num,
    /// Acquire a dirty page
    pub grabbed_dirty: Num,
    /// Acquire a clean page
    pub grabbed_clean: Num,
}

lazy_static! {
    pub static ref THREAD_CTR: AtomicUsize = AtomicUsize::new(0);
}

#[allow(dead_code)]
pub struct StatsHandle {
    pub stats: RefCell<AllocStats>,
    pub thread_num: usize,
}

thread_local! {
    pub static LOCAL_STATS: StatsHandle = StatsHandle {
        stats: RefCell::new(AllocStats::default()),
        thread_num: THREAD_CTR.fetch_add(1, Ordering::Relaxed),
    };
}

macro_rules! trace_event {
    ($fld:tt) => {
        #[cfg(feature = "print_stats")]
        {
            let _ = super::stats::LOCAL_STATS.try_with(|sh| {
                if let Ok(mut _f_ref) = sh.stats.try_borrow_mut() {
                    _f_ref.$fld += 1;
                    let allocs = _f_ref.slag_alloc + _f_ref.cache_alloc;
                    if (allocs % (1 << 22)) == 0 {
                        trace!("thread {:2?} - {:?}", sh.thread_num, *_f_ref);
                    }
                }
            });
        }
    };
}
