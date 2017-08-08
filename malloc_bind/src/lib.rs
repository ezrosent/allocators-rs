#![no_std]
#![feature(allocator_api)]
#![feature(alloc)]
#![feature(core_intrinsics)]
#![feature(const_fn)]

extern crate alloc;
extern crate libc;
extern crate errno;
extern crate sysconf;
// lazy_static is only used in macros, so if no macros are called, it will appear unused.
#[allow(unused)]
#[macro_use]
extern crate lazy_static;
use alloc::allocator::{Alloc, AllocErr, Layout};

use libc::{c_void, size_t};
use core::{mem, ptr};
use core::cmp::max;

const WORD_SIZE: usize = mem::size_of::<*mut c_void>();

pub trait LayoutFinder {
    fn get_layout(&self, ptr: *mut u8) -> Layout;
    fn insert_layout(&self, _ptr: *mut u8, _layout: Layout) {}
    fn delete_layout(&self, _ptr: *mut u8) {}
}

pub struct Malloc<A, L: LayoutFinder>
    where for<'a> &'a A: Alloc
{
    alloc: A,
    layout_finder: L,
}

impl<A, L: LayoutFinder> Malloc<A, L>
    where for<'a> &'a A: Alloc
{
    pub const fn new(alloc: A, layout_finder: L) -> Malloc<A, L> {
        Malloc {
            alloc,
            layout_finder,
        }
    }

    pub unsafe fn malloc(&self, size: size_t) -> *mut c_void {
        if size == 0 {
            return ptr::null_mut();
        }

        // According to the posix_memalign manpage, "The glibc malloc(3) always returns 8-byte
        // aligned memory addresses..." Thus, we round up the size of allocations to 8 bytes in
        // order guarantee that 8 is a valid alignment (since Layout requires that the size is a
        // multiple of the alignment).
        let size = max(size, 8);

        let layout = Layout::from_size_align(size as usize, 8).unwrap();
        match (&self.alloc).alloc(layout.clone()) {
            Ok(ptr) => {
                self.layout_finder.insert_layout(ptr, layout);
                ptr as *mut c_void
            }
            Err(AllocErr::Exhausted { .. }) => ptr::null_mut(),
            Err(AllocErr::Unsupported { .. }) => core::intrinsics::abort(),
        }
    }

    pub unsafe fn free(&self, ptr: *mut c_void) {
        if ptr.is_null() {
            return;
        }

        let layout = self.layout_finder.get_layout(ptr as *mut u8);
        self.layout_finder.delete_layout(ptr as *mut u8);
        (&self.alloc).dealloc(ptr as *mut u8, layout);
    }

    pub unsafe fn cfree(&self, ptr: *mut c_void) {
        // See https://linux.die.net/man/3/cfree
        self.free(ptr)
    }

    pub unsafe fn calloc(&self, nmemb: size_t, size: size_t) -> *mut c_void {
        if nmemb == 0 || size == 0 {
            return ptr::null_mut();
        }

        // According to the posix_memalign manpage, "The glibc malloc(3) always returns 8-byte
        // aligned memory addresses..." Thus, we round up the size of allocations to 8 bytes in
        // order guarantee that 8 is a valid alignment (since Layout requires that the size is a
        // multiple of the alignment).
        let size = max(size, 8);

        let layout = Layout::from_size_align(nmemb * size as usize, 8).unwrap();
        match (&self.alloc).alloc_zeroed(layout.clone()) {
            Ok(ptr) => {
                self.layout_finder.insert_layout(ptr, layout);
                ptr as *mut c_void
            }
            Err(AllocErr::Exhausted { .. }) => ptr::null_mut(),
            Err(AllocErr::Unsupported { .. }) => core::intrinsics::abort(),
        }
    }

    // NOTE: Obsolete.
    pub unsafe fn valloc(&self, size: size_t) -> *mut c_void {
        if size == 0 {
            return ptr::null_mut();
        }

        let layout = Layout::from_size_align(size as usize, sysconf::pagesize()).unwrap();
        match (&self.alloc).alloc_zeroed(layout.clone()) {
            Ok(ptr) => {
                self.layout_finder.insert_layout(ptr, layout);
                ptr as *mut c_void
            }
            Err(AllocErr::Exhausted { .. }) => ptr::null_mut(),
            Err(AllocErr::Unsupported { .. }) => core::intrinsics::abort(),
        }
    }

    // NOTE: Obsolete.
    #[cfg(target_os = "linux")]
    pub unsafe fn pvalloc(&self, size: size_t) -> *mut c_void {
        // See http://man7.org/linux/man-pages/man3/posix_memalign.3.html

        if size == 0 {
            return ptr::null_mut();
        }

        // TODO: round size up to the next multiple of the page size.

        let layout = Layout::from_size_align(size as usize, sysconf::pagesize()).unwrap();
        match (&self.alloc).alloc_zeroed(layout.clone()) {
            Ok(ptr) => {
                self.layout_finder.insert_layout(ptr, layout);
                ptr as *mut c_void
            }
            Err(AllocErr::Exhausted { .. }) => ptr::null_mut(),
            Err(AllocErr::Unsupported { .. }) => core::intrinsics::abort(),
        }
    }

    pub unsafe fn realloc(&self, ptr: *mut c_void, size: size_t) -> *mut c_void {
        // See http://man7.org/linux/man-pages/man3/malloc.3.html,
        // http://www.manpagez.com/man/3/malloc/osx-10.6.php

        if ptr.is_null() {
            return self.malloc(size);
        }

        if size == 0 {
            // According to the Linux manpage: "if size is equal to zero, and ptr is not NULL, then
            // the call is equivalent to free(ptr)." However, according to Darwin: "If size is zero
            // and ptr is not NULL, a new, minimum sized object is allocated and the original
            // object is freed." Since it is valid for malloc(0) to simply return NULL, we opt to
            // implement the Linux behavior in both cases. The only way for this to cause problems
            // is for Darwin programs to rely on the fact that the returned pointer represents the
            // "minimum sized object" instead of only assuming that, since the size passed was 0,
            // the object has 0 size. Since "minimum sized object" does not seem to be a
            // well-defined term, reliance on such behavior is erroneous.

            // TODO: What should we return?
            self.free(ptr);
            return ptr::null_mut();
        }

        // TODO: Round size up to 8 and use 8-byte alignment like in malloc/calloc?

        let layout = self.layout_finder.get_layout(ptr as *mut u8);
        // TODO: What's the right choice of alignment here?
        let new_layout = Layout::from_size_align(size as usize, 1).unwrap();
        match (&self.alloc).realloc(ptr as *mut u8, layout, new_layout.clone()) {
            Ok(ptr) => {
                self.layout_finder.delete_layout(ptr);
                self.layout_finder.insert_layout(ptr, new_layout);
                ptr as *mut c_void
            }
            Err(AllocErr::Exhausted { .. }) => ptr::null_mut(),
            Err(AllocErr::Unsupported { .. }) => core::intrinsics::abort(),
        }
    }

    #[cfg(target_os = "macos")]
    pub unsafe fn reallocf(&self, ptr: *mut c_void, size: size_t) -> *mut c_void {
        // See http://www.manpagez.com/man/3/malloc/osx-10.6.php

        if ptr.is_null() {
            return self.malloc(size);
        }

        if size == 0 {
            // According to the malloc manpage: "If size is zero and ptr is not NULL, a new,
            // minimum sized object is allocated and the original object is freed." See the
            // equivalent comment in realloc for why we do this.

            // TODO: What should we return?
            self.free(ptr);
            return ptr::null_mut();
        }

        // TODO: Round size up to 8 and use 8-byte alignment like in malloc/calloc?

        let layout = self.layout_finder.get_layout(ptr as *mut u8);
        // TODO: What's the right choice of alignment here?
        let new_layout = Layout::from_size_align(size as usize, 1).unwrap();
        match (&self.alloc).realloc(ptr as *mut u8, layout, new_layout.clone()) {
            Ok(ptr) => {
                self.layout_finder.delete_layout(ptr);
                self.layout_finder.insert_layout(ptr, new_layout);
                ptr as *mut c_void
            }
            Err(AllocErr::Exhausted { .. }) => {
                self.free(ptr);
                ptr::null_mut()
            }
            Err(AllocErr::Unsupported { .. }) => core::intrinsics::abort(),
        }
    }

    #[cfg(target_os = "linux")]
    pub unsafe fn reallocarray(&self,
                               ptr: *mut c_void,
                               nmemb: size_t,
                               size: size_t)
                               -> *mut c_void {
        // See http://man7.org/linux/man-pages/man3/malloc.3.html

        // According to the malloc manpage, "unlike that realloc() call, reallocarray() fails
        // safely in the case where the multiplication would overflow. If such an overflow occurs,
        // reallocarray() returns NULL, sets errno to ENOMEM, and leaves the original block of
        // memory unchanged."
        match nmemb.checked_mul(size) {
            Some(product) => self.realloc(ptr, product),
            None => {
                errno::set_errno(errno::Errno(libc::ENOMEM));
                ptr::null_mut()
            }
        }
    }


    pub unsafe fn posix_memalign(&self,
                                 memptr: *mut *mut c_void,
                                 alignment: size_t,
                                 size: size_t)
                                 -> i32 {
        // See http://man7.org/linux/man-pages/man3/posix_memalign.3.html

        // The manpage also specifies that the alignment must be a multiple of the word size, but
        // all powers of two greater than or equal to the word size are multiples of the word size,
        // so we omit that check.
        if alignment <= WORD_SIZE || !alignment.is_power_of_two() {
            return libc::EINVAL;
        }

        if size == 0 {
            *memptr = ptr::null_mut();
            return 0;
        }

        // TODO: posix_memalign does not require that size is a multiple of alignment. Thus, we
        // need to manually round up since valid Layouts must have that property. This is safe
        // because this API never takes the memory region size on deallocation, so it's fine that
        // the caller might think they have a smaller memory region than they actually do.

        let layout = Layout::from_size_align(size as usize, alignment).unwrap();
        match (&self.alloc).alloc(layout.clone()) {
            Ok(ptr) => {
                self.layout_finder.insert_layout(ptr, layout);
                *memptr = ptr as *mut c_void;
                0
            }
            Err(AllocErr::Exhausted { .. }) => libc::ENOMEM,
            Err(AllocErr::Unsupported { .. }) => core::intrinsics::abort(),
        }
    }

    // NOTE: Obsolete.
    #[cfg(target_os = "linux")]
    pub unsafe fn memalign(&self, alignment: size_t, size: size_t) -> *mut c_void {
        // See http://man7.org/linux/man-pages/man3/posix_memalign.3.html

        if !alignment.is_power_of_two() {
            return ptr::null_mut();
        }

        if size == 0 {
            return ptr::null_mut();
        }

        // TODO: memalign does not require that size is a multiple of alignment. Thus, we need to
        // manually round up since valid Layouts must have that property. This is safe because this
        // API never takes the memory region size on deallocation, so it's fine that the caller
        // might think they have a smaller memory region than they actually do.

        let layout = Layout::from_size_align(size as usize, alignment).unwrap();
        match (&self.alloc).alloc(layout.clone()) {
            Ok(ptr) => {
                self.layout_finder.insert_layout(ptr, layout);
                ptr as *mut c_void
            }
            Err(AllocErr::Exhausted { .. }) => ptr::null_mut(),
            Err(AllocErr::Unsupported { .. }) => core::intrinsics::abort(),
        }
    }

    #[cfg(target_os = "linux")]
    pub unsafe fn aligned_alloc(&self, alignment: size_t, size: size_t) -> *mut c_void {
        // See http://man7.org/linux/man-pages/man3/posix_memalign.3.html

        // From the aligned_alloc manpage: "The function aligned_alloc() is the same as memalign(),
        // except for the added restriction that size should be a multiple of alignment."
        if size % alignment != 0 {
            return ptr::null_mut();
        }
        self.memalign(alignment, size)
    }
}

#[macro_export]
macro_rules! define_malloc {
    ($alloc_ty:ty, $alloc_new:expr, $layout_finder_ty:ty, $layout_finder_new:expr) => (
        static HEAP: $crate::Malloc<$alloc_ty, $layout_finder_ty> = $crate::Malloc::new($alloc_new, $layout_finder_new);

        #[no_mangle]
        pub extern "C" fn malloc(size: size_t) -> *mut c_void {
            unsafe { HEAP.malloc(size) }
        }

        #[no_mangle]
        pub extern "C" fn free(ptr: *mut c_void) {
            unsafe { HEAP.free(ptr) }
        }

        #[no_mangle]
        pub extern "C" fn cfree(ptr: *mut c_void) {
            unsafe { HEAP.cfree(ptr) }
        }

        #[no_mangle]
        pub extern "C" fn calloc(nmemb: size_t, size: size_t) -> *mut c_void {
            unsafe { HEAP.calloc(nmemb, size) }
        }

        #[no_mangle]
        pub extern "C" fn valloc(size: size_t) -> *mut c_void {
            unsafe { HEAP.valloc(size) }
        }

        #[cfg(target_os = "linux")]
        #[no_mangle]
        pub extern "C" fn pvalloc(size: size_t) -> *mut c_void {
            unsafe { HEAP.pvalloc(size) }
        }

        #[no_mangle]
        pub extern "C" fn realloc(ptr: *mut c_void, size: size_t) -> *mut c_void {
            unsafe { HEAP.realloc(ptr, size) }
        }

        #[cfg(target_os = "macos")]
        #[no_mangle]
        pub extern "C" fn reallocf(ptr: *mut c_void, size: size_t) -> *mut c_void {
            unsafe { HEAP.reallocf(ptr, size) }
        }

        #[cfg(target_os = "linux")]
        pub extern "C" fn reallocarray(ptr: *mut c_void, nmemb: size_t, size: size_t) -> *mut c_void {
            unsafe { HEAP.reallocarray(ptr, nmemb, size) }
        }

        #[no_mangle]
        pub extern "C" fn posix_memalign(memptr: *mut *mut c_void, alignment: size_t, size: size_t) -> i32 {
            unsafe { HEAP.posix_memalign(memptr, alignment, size) }
        }

        #[cfg(target_os = "linux")]
        #[no_mangle]
        pub extern "C" fn memalign(alignment: size_t, size: size_t) -> *mut c_void {
            unsafe { HEAP.memalign(alignment, size) }
        }

        #[cfg(target_os = "linux")]
        #[no_mangle]
        pub extern "C" fn aligned_alloc(alignment: size_t, size: size_t) -> *mut c_void {
            unsafe { HEAP.aligned_alloc(alignment, size) }
        }
    )
}

// This line re-exports the macros from lazy_static so that they'll be available to the code
// calling define_malloc_lazy_static. This allows define_malloc_lazy_static to be used without the
// caller needing to know about lazy_static and import its macros themselves.
//
// Credit to https://users.rust-lang.org/t/how-to-use-macro-inside-another-macro/12061/2
#[allow(unused)]
pub use lazy_static::*;

#[macro_export]
macro_rules! define_malloc_lazy_static {
    ($alloc_ty:ty, $alloc_new:expr, $layout_finder_ty:ty, $layout_finder_new:expr) => (
        lazy_static!{
            static ref HEAP: $crate::Malloc<$alloc_ty, $layout_finder_ty> = $crate::Malloc::new($alloc_new, $layout_finder_new);
        }

        #[no_mangle]
        pub extern "C" fn malloc(size: size_t) -> *mut c_void {
            unsafe { HEAP.malloc(size) }
        }

        #[no_mangle]
        pub extern "C" fn free(ptr: *mut c_void) {
            unsafe { HEAP.free(ptr) }
        }

        #[no_mangle]
        pub extern "C" fn cfree(ptr: *mut c_void) {
            unsafe { HEAP.cfree(ptr) }
        }

        #[no_mangle]
        pub extern "C" fn calloc(nmemb: size_t, size: size_t) -> *mut c_void {
            unsafe { HEAP.calloc(nmemb, size) }
        }

        #[no_mangle]
        pub extern "C" fn valloc(size: size_t) -> *mut c_void {
            unsafe { HEAP.valloc(size) }
        }

        #[cfg(target_os = "linux")]
        #[no_mangle]
        pub extern "C" fn pvalloc(size: size_t) -> *mut c_void {
            unsafe { HEAP.pvalloc(size) }
        }

        #[no_mangle]
        pub extern "C" fn realloc(ptr: *mut c_void, size: size_t) -> *mut c_void {
            unsafe { HEAP.realloc(ptr, size) }
        }

        #[cfg(target_os = "macos")]
        #[no_mangle]
        pub extern "C" fn reallocf(ptr: *mut c_void, size: size_t) -> *mut c_void {
            unsafe { HEAP.reallocf(ptr, size) }
        }

        #[cfg(target_os = "linux")]
        pub extern "C" fn reallocarray(ptr: *mut c_void, nmemb: size_t, size: size_t) -> *mut c_void {
            unsafe { HEAP.reallocarray(ptr, nmemb, size) }
        }

        #[no_mangle]
        pub extern "C" fn posix_memalign(memptr: *mut *mut c_void, alignment: size_t, size: size_t) -> i32 {
            unsafe { HEAP.posix_memalign(memptr, alignment, size) }
        }

        #[cfg(target_os = "linux")]
        #[no_mangle]
        pub extern "C" fn memalign(alignment: size_t, size: size_t) -> *mut c_void {
            unsafe { HEAP.memalign(alignment, size) }
        }

        #[cfg(target_os = "linux")]
        #[no_mangle]
        pub extern "C" fn aligned_alloc(alignment: size_t, size: size_t) -> *mut c_void {
            unsafe { HEAP.aligned_alloc(alignment, size) }
        }
    )
}
