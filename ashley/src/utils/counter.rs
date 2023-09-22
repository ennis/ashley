//! Atomic counters
use ashley_db::{AsIndex, Index};
use std::{
    marker::PhantomData,
    sync::atomic::{AtomicU32, Ordering},
};

pub struct Counter<I: AsIndex>(AtomicU32, PhantomData<fn() -> I>);

impl<I: AsIndex> Counter<I> {
    /// Create a new counter.
    pub const fn new() -> Counter<I> {
        Counter(AtomicU32::new(0), PhantomData)
    }

    /// Return the next value.
    pub fn next(&self) -> I {
        let next = self.0.fetch_add(1, Ordering::Relaxed);
        I::from_index(Index::from_u32(next))
    }
}

impl<I: AsIndex> Default for Counter<I> {
    fn default() -> Self {
        Counter::new()
    }
}
