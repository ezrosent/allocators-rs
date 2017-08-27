//! This module includes some lightweight utilities for tracing different allocation events.
use std::cell::RefCell;

type Num = i64;

#[derive(Default, Debug, Clone, Copy)]
pub struct AllocStats {
    pub slag_alloc: Num,
    pub cache_alloc: Num,

    pub local_free: Num,
    pub remote_free: Num,
    pub bulk_remote_free: Num,

    pub transition_available: Num,
    pub transition_full: Num,

    pub grabbed_available: Num,
    pub grabbed_dirty: Num,
    pub grabbed_clean: Num,
}

#[allow(dead_code)]
pub struct StatsHandle(pub RefCell<AllocStats>);

thread_local! {
    pub static LOCAL_STATS: StatsHandle = StatsHandle(RefCell::new(AllocStats::default()));
}

macro_rules! trace_event {
    ($fld:tt) => {
        #[cfg(feature = "print_stats")]
        {
            let _ = super::stats::LOCAL_STATS.try_with(|sh| {
                use std::thread;
                let mut _f_ref = sh.0.borrow_mut();
                _f_ref.$fld += 1;
                let allocs = _f_ref.slag_alloc + _f_ref.cache_alloc;
                if (allocs+1 % (1 << 20)) == 0 {
                    trace!("thread {:?} - {:?}", thread::current().id(), *_f_ref);
                }
            });
        }
    };
}
