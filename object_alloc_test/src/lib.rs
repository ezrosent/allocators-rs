#![feature(alloc)]
#![feature(allocator_api)]
#![feature(test)]

#[cfg(unix)]
#[macro_use]
extern crate lazy_static;

pub mod corruption;
pub mod leaky_alloc;
