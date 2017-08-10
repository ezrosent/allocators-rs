// TODO:
// - Figure out how to panic without allocating

#![cfg_attr(any(not(test), feature = "test_no_std"), no_std)]
#![cfg_attr(all(test, not(feature = "test_no_std")), feature(test))]
#![feature(alloc, allocator_api)]

#[cfg(all(test, not(feature = "test_no_std")))]
extern crate core;

extern crate alloc;
extern crate libc;
extern crate sysconf;

#[cfg(any(target_os = "linux", target_os = "macos"))]
extern crate errno;

#[cfg(windows)]
extern crate kernel32;
#[cfg(windows)]
extern crate winapi;

use alloc::allocator::{Alloc, Layout, Excess, AllocErr};
use sysconf::pagesize;
use core::ptr::null_mut;

#[cfg(any(target_os = "linux", target_os = "macos"))]
use errno::errno;

pub struct MapAllocBuilder {
    read: bool,
    write: bool,
    exec: bool,
}

impl MapAllocBuilder {
    pub fn default() -> MapAllocBuilder {
        MapAllocBuilder {
            read: true,
            write: true,
            exec: false,
        }
    }

    pub fn build(&self) -> MapAlloc {
        MapAlloc(PageSizeAlloc::new(BasicPageSize,
                                    perms::get_perm(self.read, self.write, self.exec)))
    }

    pub fn build_huge(&self, hugepage_size: usize) -> HugeMapAlloc {
        #[cfg(target_os = "linux")]
        assert!(sysconf::hugepage::hugepage_supported(hugepage_size),
                "unsupported hugepage size: {}",
                hugepage_size);
        HugeMapAlloc(PageSizeAlloc::new(HugePageSize(hugepage_size),
                                        perms::get_perm(self.read, self.write, self.exec)))
    }

    #[cfg(target_os = "linux")]
    pub fn build_huge_default(&self) -> Option<HugeMapAlloc> {
        sysconf::hugepage::default_hugepage().map(|size| self.build_huge(size))
    }

    pub fn read(mut self) -> MapAllocBuilder {
        self.read = true;
        self
    }

    pub fn write(mut self) -> MapAllocBuilder {
        self.write = true;
        self
    }

    pub fn exec(mut self) -> MapAllocBuilder {
        self.exec = true;
        self
    }

    pub fn no_read(mut self) -> MapAllocBuilder {
        self.read = false;
        self
    }

    pub fn no_write(mut self) -> MapAllocBuilder {
        self.write = false;
        self
    }

    pub fn no_exec(mut self) -> MapAllocBuilder {
        self.exec = false;
        self
    }
}

struct BasicPageSize;

impl PageSize for BasicPageSize {
    fn pagesize(&self) -> usize {
        pagesize()
    }
    fn huge_pagesize(&self) -> Option<usize> {
        None
    }
}

pub struct MapAlloc(PageSizeAlloc<BasicPageSize>);

impl MapAlloc {
    pub fn new() -> MapAlloc {
        MapAllocBuilder::default().build()
    }

    #[cfg(any(target_os = "linux", target_os = "macos"))]
    pub unsafe fn uncommit(&self, ptr: *mut u8, layout: Layout) {
        self.0.uncommit(ptr, layout);
    }
}

impl Default for MapAlloc {
    fn default() -> MapAlloc {
        MapAlloc::new()
    }
}

unsafe impl Alloc for MapAlloc {
    unsafe fn alloc(&mut self, layout: Layout) -> Result<*mut u8, AllocErr> {
        self.0.alloc(layout)
    }

    unsafe fn dealloc(&mut self, ptr: *mut u8, layout: Layout) {
        self.0.dealloc(ptr, layout)
    }

    unsafe fn alloc_zeroed(&mut self, layout: Layout) -> Result<*mut u8, AllocErr> {
        self.0.alloc_zeroed(layout)
    }

    unsafe fn alloc_excess(&mut self, layout: Layout) -> Result<Excess, AllocErr> {
        self.0.alloc_excess(layout)
    }
}

unsafe impl<'a> Alloc for &'a MapAlloc {
    unsafe fn alloc(&mut self, layout: Layout) -> Result<*mut u8, AllocErr> {
        self.0.alloc(layout)
    }

    unsafe fn dealloc(&mut self, ptr: *mut u8, layout: Layout) {
        self.0.dealloc(ptr, layout)
    }

    unsafe fn alloc_zeroed(&mut self, layout: Layout) -> Result<*mut u8, AllocErr> {
        self.0.alloc_zeroed(layout)
    }

    unsafe fn alloc_excess(&mut self, layout: Layout) -> Result<Excess, AllocErr> {
        self.0.alloc_excess(layout)
    }
}

struct HugePageSize(usize);

impl PageSize for HugePageSize {
    fn pagesize(&self) -> usize {
        self.0
    }
    fn huge_pagesize(&self) -> Option<usize> {
        Some(self.0)
    }
}

pub struct HugeMapAlloc(PageSizeAlloc<HugePageSize>);

impl HugeMapAlloc {
    #[cfg(target_os = "linux")]
    pub fn new() -> Option<HugeMapAlloc> {
        MapAllocBuilder::default().build_huge_default()
    }

    #[cfg(any(target_os = "linux", target_os = "macos"))]
    pub unsafe fn uncommit(&self, ptr: *mut u8, layout: Layout) {
        self.0.uncommit(ptr, layout);
    }
}

unsafe impl Alloc for HugeMapAlloc {
    unsafe fn alloc(&mut self, layout: Layout) -> Result<*mut u8, AllocErr> {
        self.0.alloc(layout)
    }

    unsafe fn dealloc(&mut self, ptr: *mut u8, layout: Layout) {
        self.0.dealloc(ptr, layout)
    }

    unsafe fn alloc_zeroed(&mut self, layout: Layout) -> Result<*mut u8, AllocErr> {
        self.0.alloc_zeroed(layout)
    }

    unsafe fn alloc_excess(&mut self, layout: Layout) -> Result<Excess, AllocErr> {
        self.0.alloc_excess(layout)
    }
}

unsafe impl<'a> Alloc for &'a HugeMapAlloc {
    unsafe fn alloc(&mut self, layout: Layout) -> Result<*mut u8, AllocErr> {
        self.0.alloc(layout)
    }

    unsafe fn dealloc(&mut self, ptr: *mut u8, layout: Layout) {
        self.0.dealloc(ptr, layout)
    }

    unsafe fn alloc_zeroed(&mut self, layout: Layout) -> Result<*mut u8, AllocErr> {
        self.0.alloc_zeroed(layout)
    }

    unsafe fn alloc_excess(&mut self, layout: Layout) -> Result<Excess, AllocErr> {
        self.0.alloc_excess(layout)
    }
}

trait PageSize {
    fn pagesize(&self) -> usize;
    fn huge_pagesize(&self) -> Option<usize>;
}

struct PageSizeAlloc<P: PageSize> {
    pagesize: P,
    perms: perms::Perm,
}

impl<P: PageSize> PageSizeAlloc<P> {
    fn new(pagesize: P, perms: perms::Perm) -> PageSizeAlloc<P> {
        PageSizeAlloc {
            pagesize: pagesize,
            perms: perms,
        }
    }

    fn alloc(&self, layout: Layout) -> Result<*mut u8, AllocErr> {
        self.alloc_excess(layout).map(|Excess(ptr, _)| ptr)
    }

    fn dealloc(&self, ptr: *mut u8, layout: Layout) {
        munmap(ptr, layout.size());
    }

    fn alloc_zeroed(&self, layout: Layout) -> Result<*mut u8, AllocErr> {
        self.alloc(layout)
    }

    fn alloc_excess(&self, layout: Layout) -> Result<Excess, AllocErr> {
        // alignment less than a page is fine because page-aligned objects are also aligned to
        // any alignment less than a page
        if layout.align() > self.pagesize.pagesize() {
            return Err(AllocErr::invalid_input("cannot support alignment greater than a page"));
        }

        let size = next_multiple(layout.size(), self.pagesize.pagesize());
        match self.alloc_helper(size) {
            Some(ptr) => Ok(Excess(ptr, size)),
            None => Err(AllocErr::Exhausted { request: layout }),
        }
    }

    // alloc_helper performs the requested allocation, properly handling the case in which mmap
    // returns null.
    fn alloc_helper(&self, size: usize) -> Option<*mut u8> {
        // Since allocators in Rust are not allowed to return null pointers, but it is valid for
        // mmap to return memory starting at null, we have to handle that case. We do this by
        // checking for null, and if we find that mmap has returned null, we unmap all but the
        // first page and try again. Since we leave the first page (the one starting at address 0)
        // mapped, future calls to mmap are guaranteed to not return null. Note that this leaks
        // memory since we never unmap that page, but this isn't a big deal - even if the page is a
        // huge page, since we never write to it, it will remain uncommitted and will thus not
        // consume any physical memory.
        let f = |ptr: *mut u8| if ptr.is_null() {
            let unmap_size = size - self.pagesize.pagesize();
            if unmap_size > 0 {
                munmap(self.pagesize.pagesize() as *mut u8, unmap_size);
            }
            // a) Make it more likely that the kernel will not keep the page backed by physical
            // memory and, b) make it so that an access to that range will result in a segfault to
            // make other bugs easier to detect.
            #[cfg(any(target_os = "linux", target_os = "macos"))]
            mark_unused(null_mut(), self.pagesize.pagesize());
            self.alloc_helper(size)
        } else {
            Some(ptr)
        };
        mmap(size, self.perms, self.pagesize.huge_pagesize()).and_then(f)
    }

    #[cfg(any(target_os = "linux", target_os = "macos"))]
    fn uncommit(&self, ptr: *mut u8, layout: Layout) {
        // TODO: What to do about sizes that are not multiples of the page size? These are legal
        // allocations, and so they are legal to pass to uncommit, but will madvise handle them
        // properly?
        if let Some(huge) = self.pagesize.huge_pagesize() {
            debug_assert_eq!(ptr as usize % huge, 0);
            debug_assert!(layout.align() <= huge);
        } else {
            debug_assert_eq!(ptr as usize % self.pagesize.pagesize(), 0);
            debug_assert!(layout.align() <= self.pagesize.pagesize());
        }
        uncommit(ptr, layout.size());
    }
}

fn next_multiple(size: usize, unit: usize) -> usize {
    if size % unit == 0 {
        size
    } else {
        size + (size - (size % unit))
    }
}

#[cfg(target_os = "linux")]
fn mark_unused(ptr: *mut u8, size: usize) {
    use libc::{c_void, MADV_DONTNEED, PROT_NONE};
    unsafe {
        // Let the kernel know we don't need this memory, so it can free physical resources for it
        libc::madvise(ptr as *mut c_void, size, MADV_DONTNEED);
        // Make it so that accesses to this memory result in a segfault
        libc::mprotect(ptr as *mut c_void, size, PROT_NONE);
    }
}

#[cfg(target_os = "macos")]
fn mark_unused(ptr: *mut u8, size: usize) {
    use libc::{c_void, MADV_FREE, PROT_NONE};
    unsafe {
        // Let the kernel know we don't need this memory, so it can free physical resources for it
        libc::madvise(ptr as *mut c_void, size, MADV_FREE);
        // Make it so that accesses to this memory result in a segfault
        libc::mprotect(ptr as *mut c_void, size, PROT_NONE);
    }
}

#[cfg(target_os = "linux")]
fn mmap(size: usize, perms: i32, huge_pagesize: Option<usize>) -> Option<*mut u8> {
    use libc::{MAP_ANONYMOUS, MAP_PRIVATE, MAP_HUGETLB, MAP_FAILED, ENOMEM};

    // TODO: Figure out when it's safe to pass MAP_UNINITIALIZED (it's not defined in all
    // versions of libc). Be careful about not invalidating alloc_zeroed.

    // MAP_HUGE_SHIFT isn't used on all kernel versions, but I assume it must be
    // backwards-compatible. The only way for it to not be backwards-compatible would be for
    // there to be bits in the range [26, 31] (in the 'flags' argument) that used to be
    // meaningful. That would make old programs fail on newer kernels. In theory, old kernels
    // could be checking to make sure that undefined flags aren't set, but that seems unlikely.
    // See:
    // http://elixir.free-electrons.com/linux/latest/source/arch/alpha/include/uapi/asm/mman.h
    // http://man7.org/linux/man-pages/man2/mmap.2.html
    const MAP_HUGE_SHIFT: usize = 26;
    let flags = if let Some(pagesize) = huge_pagesize {
        debug_assert!(pagesize.is_power_of_two()); // implies pagesize > 0
        let log = pagesize.trailing_zeros();
        MAP_HUGETLB | ((log as i32) << MAP_HUGE_SHIFT)
    } else {
        0
    };

    let ptr = unsafe {
        libc::mmap(null_mut(),
                   size,
                   perms,
                   MAP_ANONYMOUS | MAP_PRIVATE | flags,
                   -1,
                   0)
    };

    if ptr == MAP_FAILED {
        if errno().0 == ENOMEM {
            None
        } else {
            panic!("mmap failed: {}", errno())
        }
    } else {
        Some(ptr as *mut u8)
    }
}

#[cfg(target_os = "macos")]
fn mmap(size: usize, perms: i32, huge_pagesize: Option<usize>) -> Option<*mut u8> {
    use libc::{MAP_ANON, MAP_PRIVATE, MAP_FAILED, ENOMEM};

    // TODO: Support superpages (see MAP_ANON description in mmap manpage)
    debug_assert!(huge_pagesize.is_none());

    let ptr = unsafe { libc::mmap(null_mut(), size, perms, MAP_ANON | MAP_PRIVATE, -1, 0) };

    if ptr == MAP_FAILED {
        if errno().0 == ENOMEM {
            None
        } else {
            panic!("mmap failed: {}", errno())
        }
    } else {
        Some(ptr as *mut u8)
    }
}

// For a good overview of virtual memory handling on Windows, see
// https://blogs.technet.microsoft.com/markrussinovich/2008/11/17/pushing-the-limits-of-windows-virtual-memory/

#[cfg(windows)]
fn mmap(size: usize, perms: u32, huge_pagesize: Option<usize>) -> Option<*mut u8> {
    use kernel32::VirtualAlloc;
    use winapi::winnt::{MEM_RESERVE, MEM_COMMIT, MEM_LARGE_PAGES};

    let typ = if huge_pagesize.is_none() {
        MEM_RESERVE | MEM_COMMIT
    } else {
        MEM_RESERVE | MEM_COMMIT | MEM_LARGE_PAGES
    };

    unsafe {
        // NOTE: While Windows makes a distinction between allocation granularity and page size
        // (see https://msdn.microsoft.com/en-us/library/windows/desktop/ms724958(v=vs.85).aspx),
        // VirtualAlloc only cares about allocation granularity for the pointer argument, not the
        // size. Since we're passing null for the pointer, this doesn't affect us.
        let ptr = VirtualAlloc(null_mut(), size as u64, typ, perms) as *mut u8;
        // NOTE: Windows can return many different error codes in different scenarios that all
        // relate to being out of memory. Instead of trying to list them all, we assume that any
        // error is an out-of-memory condition. This is fine so long as our code doesn't have a bug
        // (that would, e.g., result in VirtualAlloc being called with invalid arguments). This
        // isn't ideal, but during debugging, error codes can be printed here, so it's not the end
        // of the world.
        if ptr.is_null() { None } else { Some(ptr) }
    }
}

#[cfg(any(target_os = "linux", target_os = "macos"))]
fn munmap(ptr: *mut u8, size: usize) {
    use libc::{munmap, c_void};
    unsafe {
        assert_eq!(munmap(ptr as *mut c_void, size), 0);
    }
}

#[cfg(windows)]
fn munmap(ptr: *mut u8, _size: usize) {
    use kernel32::{VirtualFree, GetLastError};
    use winapi::winnt::MEM_RELEASE;

    unsafe {
        // NOTE: VirtualFree, when unmapping memory (as opposed to decommitting it), can only
        // operate on an entire region previously mapped with VirtualAlloc. As a result, 'ptr' must
        // have been previously returned by VirtualAlloc, and no length is needed since it is known
        // by the kernel (VirtualFree /requires/ that if the third argument is MEM_RELEASE, the
        // second is 0).
        let ret = VirtualFree(ptr as *mut winapi::c_void, 0, MEM_RELEASE);
        if ret == 0 {
            panic!("Call to VirtualFree failed with error code {}.",
                   GetLastError());
        }
    }
}

#[cfg(target_os = "linux")]
fn uncommit(ptr: *mut u8, size: usize) {
    use libc::{c_void, MADV_DONTNEED};
    unsafe {
        // TODO: Other options such as MADV_FREE are available on newer versions of Linux. Is there
        // a way that we can use those when available? Is that even desirable?
        libc::madvise(ptr as *mut c_void, size, MADV_DONTNEED);
    }
}

#[cfg(target_os = "macos")]
fn uncommit(ptr: *mut u8, size: usize) {
    use libc::{c_void, MADV_FREE};
    unsafe {
        libc::madvise(ptr as *mut c_void, size, MADV_FREE);
    }
}

mod perms {
    #[cfg(any(target_os = "linux", target_os = "macos"))]
    pub use self::unix::*;
    #[cfg(any(target_os = "linux", target_os = "macos"))]
    pub type Perm = i32;
    #[cfg(windows)]
    pub use self::windows::*;
    #[cfg(windows)]
    pub type Perm = u32;

    pub fn get_perm(read: bool, write: bool, exec: bool) -> Perm {
        match (read, write, exec) {
            (false, false, false) => PROT_NONE,
            (true, false, false) => PROT_READ,
            (false, true, false) => PROT_WRITE,
            (false, false, true) => PROT_EXEC,
            (true, true, false) => PROT_READ_WRITE,
            (true, false, true) => PROT_READ_EXEC,
            (false, true, true) => PROT_WRITE_EXEC,
            (true, true, true) => PROT_READ_WRITE_EXEC,
        }
    }

    #[cfg(any(target_os = "linux", target_os = "macos"))]
    mod unix {
        // NOTE: On some platforms, libc::PROT_WRITE may imply libc::PROT_READ, and libc::PROT_READ
        // may imply libc::PROT_EXEC.
        extern crate libc;
        pub const PROT_NONE: i32 = libc::PROT_NONE;
        pub const PROT_READ: i32 = libc::PROT_READ;
        pub const PROT_WRITE: i32 = libc::PROT_WRITE;
        pub const PROT_EXEC: i32 = libc::PROT_EXEC;
        pub const PROT_READ_WRITE: i32 = libc::PROT_READ | libc::PROT_WRITE;
        pub const PROT_READ_EXEC: i32 = libc::PROT_READ | libc::PROT_EXEC;
        pub const PROT_WRITE_EXEC: i32 = libc::PROT_WRITE | libc::PROT_EXEC;
        pub const PROT_READ_WRITE_EXEC: i32 = libc::PROT_READ | libc::PROT_WRITE | libc::PROT_EXEC;
    }

    #[cfg(windows)]
    mod windows {
        extern crate winapi;
        use self::winapi::winnt;
        pub const PROT_NONE: u32 = winnt::PAGE_NOACCESS;
        pub const PROT_READ: u32 = winnt::PAGE_READONLY;
        // windows doesn't have a write-only permission, so write implies read
        pub const PROT_WRITE: u32 = winnt::PAGE_READWRITE;
        pub const PROT_EXEC: u32 = winnt::PAGE_EXECUTE;
        pub const PROT_READ_WRITE: u32 = winnt::PAGE_READWRITE;
        pub const PROT_READ_EXEC: u32 = winnt::PAGE_EXECUTE_READ;
        // windows doesn't have a write/exec permission, so write/exec implies read/write/exec
        pub const PROT_WRITE_EXEC: u32 = winnt::PAGE_EXECUTE_READWRITE;
        pub const PROT_READ_WRITE_EXEC: u32 = winnt::PAGE_EXECUTE_READWRITE;
    }
}

#[cfg(test)]
mod tests {
    extern crate sysconf;
    use sysconf::pagesize;
    use super::*;
    use super::perms::*;


    #[cfg(not(feature = "test_no_std"))]
    extern crate test;

    // allow(unused) because these imports aren't used on windows
    #[allow(unused)]
    #[cfg(not(feature = "test_no_std"))]
    use std::time::{Instant, Duration};
    #[allow(unused)]
    #[cfg(not(feature = "test_no_std"))]
    use self::test::Bencher;

    // NOTE: Technically mmap is allowed to return 0, but (according to our empirical experience)
    // it only does this for very large map sizes (on Linux, at least 2^30 bytes). We never request
    // maps that large, so it's OK to check for null here. Even if it spuriously fails in the
    // future, it will queue us into the fact that our assumptions about when mmap returns null are
    // wrong.
    fn test_valid_map_address(ptr: *mut u8) {
        assert!(ptr as usize > 0, "ptr: {:?}", ptr);
        assert!(ptr as usize % pagesize() == 0, "ptr: {:?}", ptr);
    }

    // Test that the given range is readable and initialized to zero.
    fn test_zero_filled(ptr: *mut u8, size: usize) {
        for i in 0..size {
            unsafe {
                assert_eq!(*ptr.offset(i as isize), 0);
            }
        }
    }

    // Test that the given range is writable.
    fn test_write(ptr: *mut u8, size: usize) {
        for i in 0..size {
            unsafe {
                *ptr.offset(i as isize) = 1;
            }
        }
    }

    // Test that the given range is readable and writable, and that writes can be read back.
    fn test_write_read(ptr: *mut u8, size: usize) {
        for i in 0..size {
            unsafe {
                *ptr.offset(i as isize) = 1;
            }
        }
        for i in 0..size {
            unsafe {
                assert_eq!(*ptr.offset(i as isize), 1);
            }
        }
    }

    #[test]
    fn test_map() {
        // Check that:
        // - Mapping a single page works
        // - The returned pointer is non-null
        // - The returned pointer is page-aligned
        // - The page is zero-filled
        // - Unmapping it after it's already been unmapped is OK (except on windows).
        let mut ptr = mmap(pagesize(), PROT_READ_WRITE, None).unwrap();
        test_valid_map_address(ptr);
        test_zero_filled(ptr, pagesize());
        munmap(ptr, pagesize());
        #[cfg(not(windows))]
        munmap(ptr, pagesize());

        // Check that:
        // - Mapping multiple pages work
        // - The returned pointer is non-null
        // - The returned pointer is page-aligned
        // - The pages are zero-filled
        // - Unmapping it after it's already been unmapped is OK (except on windows).
        ptr = mmap(16 * pagesize(), PROT_READ_WRITE, None).unwrap();
        test_valid_map_address(ptr);
        test_zero_filled(ptr, 16 * pagesize());
        munmap(ptr, 16 * pagesize());
        #[cfg(not(windows))]
        munmap(ptr, 16 * pagesize());
    }

    #[cfg(not(windows))]
    #[test]
    fn test_map_non_windows() {
        // Check that:
        // - Unmapping a subset of a previously-mapped region works
        // - The remaining pages are still mapped
        let mut ptr = mmap(5 * pagesize(), PROT_READ_WRITE, None).unwrap();
        test_valid_map_address(ptr);
        test_zero_filled(ptr, 5 * pagesize());
        munmap(ptr, pagesize());
        munmap(unsafe { ptr.offset(2 * pagesize() as isize) }, pagesize());
        munmap(unsafe { ptr.offset(4 * pagesize() as isize) }, pagesize());
        test_zero_filled(unsafe { ptr.offset(1 * pagesize() as isize) }, pagesize());
        test_zero_filled(unsafe { ptr.offset(3 * pagesize() as isize) }, pagesize());

        // Check that:
        // - Mapping a vast region of memory works and is fast
        // - The returned pointer is non-null
        // - The returned pointer is page-aligned
        // - A read in the middle of mapping succeds and is zero

        // NOTE: Pick 2^29 bytes because, on Linux, 2^30 causes mmap to return null, which breaks
        // test_valid_map_address.
        let size = 1 << 29;
        #[cfg(not(feature = "test_no_std"))]
        let t0 = Instant::now();
        ptr = mmap(size, PROT_READ_WRITE, None).unwrap();
        #[cfg(not(feature = "test_no_std"))]
        {
            // In tests on a 2016 MacBook Pro (see bench_large_mmap), a 2^31 byte map/unmap pair
            // took ~5 usec natively (Mac OS X) and ~350 ns in a Linux VM. Thus, 1 ms is a safe
            // upper bound.
            let diff = Instant::now().duration_since(t0);
            let target = Duration::from_millis(1);
            assert!(diff < target, "duration: {:?}", diff);
        }
        test_valid_map_address(ptr);
        test_zero_filled(unsafe { ptr.offset((size / 2) as isize) }, pagesize());
        munmap(ptr, size);
    }

    #[test]
    fn test_perms() {
        // TODO: Add tests for executable permissions

        // Check that:
        // - Mapping a single read-only page works
        // - The returned pointer is non-null
        // - The returned pointer is page-aligned
        // - We can read the page, and it is zero-filled
        let mut ptr = mmap(pagesize(), PROT_READ, None).unwrap();
        test_valid_map_address(ptr);
        test_zero_filled(ptr, pagesize());
        munmap(ptr, pagesize());

        // Check that:
        // - Mapping a single write-only page works
        // - The returned pointer is non-null
        // - The returned pointer is page-aligned
        // - We can write to the page
        ptr = mmap(pagesize(), PROT_WRITE, None).unwrap();
        test_valid_map_address(ptr);
        test_write(ptr, pagesize());
        munmap(ptr, pagesize());

        // Check that:
        // - Mapping a single read-write page works
        // - The returned pointer is non-null
        // - The returned pointer is page-aligned
        // - We can read the page, and it is zero-filled
        // - We can write to the page, and those writes are properly read back
        ptr = mmap(pagesize(), PROT_READ_WRITE, None).unwrap();
        test_valid_map_address(ptr);
        test_zero_filled(ptr, pagesize());
        test_write_read(ptr, pagesize());
        munmap(ptr, pagesize());
    }

    #[cfg(not(windows))]
    #[test]
    #[should_panic]
    fn test_map_panic_zero() {
        // Check that zero length causes mmap to panic. On Windows, our mmap implementation never
        // panics.
        mmap(0, PROT_READ_WRITE, None);
    }

    #[cfg(all(not(all(target_os = "linux", target_pointer_width = "64")), not(windows)))]
    #[test]
    #[should_panic]
    fn test_map_panic_too_large() {
        // Check that an overly large length causes mmap to panic. On Windows, our mmap
        // implementation never panics. On 64-bit Linux, mmap simply responds to overly large mmaps
        // by returning ENOMEM.
        use core::usize::MAX;
        mmap(MAX, PROT_READ_WRITE, None);
    }

    #[cfg(not(windows))]
    #[test]
    #[should_panic]
    fn test_unmap_panic_zero() {
        // Check that zero length causes munmap to panic. On Windows, the length parameter is
        // ignored, so the page will simply be unmapped normally.

        // NOTE: This test leaks memory, but it's only a page, so it doesn't really matter.
        let ptr = mmap(pagesize(), PROT_READ_WRITE, None).unwrap();
        munmap(ptr, 0);
    }

    #[test]
    #[should_panic]
    fn test_unmap_panic_unaligned() {
        // Check that a non-page-aligned address causes munmap to panic.
        munmap((pagesize() / 2) as *mut u8, pagesize());
    }

    #[cfg(not(windows))]
    #[cfg(not(feature = "test_no_std"))]
    #[bench]
    #[ignore]
    fn bench_large_mmap(b: &mut Bencher) {
        // Determine the speed of mapping a large region of memory so that we can tune the timeout
        // in test_map_non_windows.
        b.iter(|| {
                   let ptr = mmap(1 << 29, PROT_READ_WRITE, None).unwrap();
                   munmap(ptr, 1 << 29);
               })
    }
}
