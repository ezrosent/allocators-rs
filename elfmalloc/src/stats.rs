//! This module includes some lightweight utilities for tracing different allocation events.
use std::cell::RefCell;
use std::sync::mpsc::{Sender, channel};
use std::sync::Mutex;
use std::thread;

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

lazy_static! {
    static ref STATS_CHAN: Mutex<Sender<AllocStats>> = {
        let (sender, receiver) = channel();
        thread::spawn(move || {
            loop {
                if let Ok(msg) = receiver.recv() {
                    println!("{:?}", msg);
                }
            }
        });
        Mutex::new(sender)
    };
}

pub struct StatsHandle(pub RefCell<AllocStats>);

impl Drop for StatsHandle {
    fn drop(&mut self) {
        let _ = STATS_CHAN.lock().unwrap().send(*self.0.borrow());
    }
}

thread_local! {
    pub static LOCAL_STATS: StatsHandle = StatsHandle(RefCell::new(AllocStats::default()));
}

macro_rules! trace_event {
    ($fld:tt) => {
        super::stats::LOCAL_STATS.with(|sh| { (sh.0.borrow_mut()).$fld += 1; });
    };
}
