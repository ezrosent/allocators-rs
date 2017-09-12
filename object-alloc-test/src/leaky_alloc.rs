// Copyright 2017 the authors. See the 'Copyright and license' section of the
// README.md file at the top-level directory of this repository.
//
// Licensed under the Apache License, Version 2.0 (the LICENSE-APACHE file) or
// the MIT license (the LICENSE-MIT file) at your option. This file may not be
// copied, modified, or distributed except according to those terms.

extern crate alloc;
use self::alloc::heap::{Alloc, AllocErr, Heap, Layout};

/// An allocator that only frees memory when it is dropped.
///
/// `LeakyAlloc` is an allocator whose `dealloc` method doesn't actually free memory, but simply
/// retains any dealloc'd memory until the `LeakyAlloc` itself is dropped. It is useful for
/// testing.
#[derive(Default)]
pub struct LeakyAlloc {
    allocs: Vec<Ptr>,
}

struct Ptr {
    ptr: *mut u8,
    layout: Layout,
}

impl Drop for Ptr {
    fn drop(&mut self) {
        unsafe {
            Alloc::dealloc(&mut Heap, self.ptr, self.layout.clone());
        }
    }
}

impl LeakyAlloc {
    /// Creates a new `LeakyAlloc`.
    pub fn new() -> LeakyAlloc {
        LeakyAlloc { allocs: Vec::new() }
    }
}

unsafe impl Alloc for LeakyAlloc {
    unsafe fn alloc(&mut self, layout: Layout) -> Result<*mut u8, AllocErr> {
        let ptr = Alloc::alloc(&mut Heap, layout.clone())?;

        self.allocs
            .push(Ptr {
                      ptr: ptr,
                      layout: layout,
                  });
        Ok(ptr)
    }

    unsafe fn dealloc(&mut self, _: *mut u8, _: Layout) {}
}

#[cfg(test)]
mod tests {
    extern crate alloc;
    extern crate core;
    extern crate object_alloc;

    use self::alloc::heap::{Alloc, AllocErr, Layout};
    use self::core::marker::PhantomData;
    use self::object_alloc::{Exhausted, ObjectAlloc};

    struct LeakyObjectAlloc<T: Default> {
        alloc: super::LeakyAlloc,
        _marker: PhantomData<T>,
    }

    impl<T: Default> LeakyObjectAlloc<T> {
        fn new() -> LeakyObjectAlloc<T> {
            LeakyObjectAlloc {
                alloc: super::LeakyAlloc::new(),
                _marker: PhantomData,
            }
        }
    }

    unsafe impl<T: Default> ObjectAlloc<T> for LeakyObjectAlloc<T> {
        unsafe fn alloc(&mut self) -> Result<*mut T, Exhausted> {
            let ptr = match Alloc::alloc(&mut self.alloc, Layout::new::<T>()) {
                Ok(ptr) => ptr as *mut T,
                Err(AllocErr::Exhausted { .. }) => return Err(Exhausted),
                Err(AllocErr::Unsupported { details }) => {
                    unreachable!("unexpected unsupported alloc: {}", details)
                }
            };

            use self::core::ptr::write;
            write(ptr, T::default());
            Ok(ptr)
        }

        unsafe fn dealloc(&mut self, ptr: *mut T) {
            use self::core::ptr::drop_in_place;
            drop_in_place(ptr);
            Alloc::dealloc(&mut self.alloc, ptr as *mut u8, Layout::new::<T>());
        }
    }

    #[test]
    fn test_memory_corruption() {
        use corruption::{CorruptionTesterDefault, TestBuilder};
        TestBuilder::new(|| LeakyObjectAlloc::<CorruptionTesterDefault>::new()).test();
    }
}
