#![feature(alloc)]
#![feature(allocator_api)]
#![feature(test)]
#![feature(const_fn)]

#[cfg(unix)]
#[macro_use]
extern crate lazy_static;

pub mod corruption;
pub mod leaky_alloc;
