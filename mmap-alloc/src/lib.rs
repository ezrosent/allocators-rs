// Copyright 2017 the authors. See the 'Copyright and license' section of the
// README.md file at the top-level directory of this repository.
//
// Licensed under the Apache License, Version 2.0 (the LICENSE-APACHE file) or
// the MIT license (the LICENSE-MIT file) at your option. This file may not be
// copied, modified, or distributed except according to those terms.

// TODO:
// - Figure out how to panic without allocating
// - Support all Unices, not just Linux and Mac
// - Add tests for UntypedObjectAlloc impls

#![cfg_attr(any(not(test), feature = "test-no-std"), no_std)]
#![cfg_attr(all(test, not(feature = "test-no-std")), feature(test))]
#![feature(alloc, allocator_api)]

#[cfg(not(any(target_os = "linux", target_os = "macos", windows)))]
compile_error!("mmap-alloc only supports Windows, Linux, and Mac");

#[cfg(all(test, not(feature = "test-no-std")))]
extern crate core;

extern crate alloc;
#[cfg(not(windows))]
extern crate libc;
extern crate object_alloc;
extern crate sysconf;

#[cfg(any(target_os = "linux", target_os = "macos"))]
extern crate errno;

#[cfg(windows)]
extern crate kernel32;
#[cfg(windows)]
extern crate winapi;

use self::alloc::allocator::{Alloc, Layout, Excess, AllocErr, CannotReallocInPlace};
use self::object_alloc::{Exhausted, UntypedObjectAlloc};
use core::ptr;

#[cfg(any(target_os = "linux", target_os = "macos"))]
use errno::errno;

/// A builder for `MapAlloc`.
///
/// `MapAllocBuilder` represents the configuration of a `MapAlloc`. New `MapAllocBuilder`s are
/// constructed using `default`, and then various other methods are used to set various
/// configuration options.
///
/// # Memory Permissions
///
/// One aspect that can be configured is the permissions of allocated memory - readable, writable,
/// or executable. By default, memory is readable and writable but not executable. Note that not
/// all combinations of permissions are supported on all platforms, and if a particular combination
/// is not supported, then another combination that is no more restrictive than the requested
/// combination will be used. For example, if execute-only permission is requested, but that is not
/// supported, then read/execute, write/execute, or read/write/execute permissions may be used. The
/// only guarantee that is made is that if the requested combination is supported on the runtime
/// platform, then precisely that configuration will be used.
///
/// Here are the known limitations with respect to permissions. This list is not guaranteed to be
/// exhaustive:
///
/// - Unix: On some platforms, write permission may imply read permission (so write and
///   write/execute are not supported), and read permission may imply execute permission (so read
///   and read/write are not supported).
/// - Windows:
///   - Write permission is not supported; it is implemented as read/write.
///   - Write/execute permission is not supported; it is implemented as read/write/execute.
///
/// ## A warning about executable memory
///
/// When allocating memory for the purpose of storing executable code, be careful about instruction
/// cache incoherency. Most CPUs keep a cache of recently-executed instructions, and writing to
/// executable memory will often not cause this cache to be invalidated, leading to surprising
/// behavior such as old code being executed even after new code has been written to memory.
///
/// In order to avoid this scenario, it is often necessary to explicitly flush the instruction
/// cache before executing newly-written memory. The mechanism to accomplish this differs by
/// system.
pub struct MapAllocBuilder {
    read: bool,
    write: bool,
    exec: bool,
    // Only supported on Linux (which has MAP_POPULATE) and Windows (which has MEM_COMMIT)
    commit: bool,
    pagesize: usize,
    huge_pagesize: Option<usize>,
    obj_size: Option<usize>,
}

impl MapAllocBuilder {
    pub fn build(&self) -> MapAlloc {
        #[cfg(target_os = "linux")]
        {
            if let Some(huge) = self.huge_pagesize {
                assert!(sysconf::page::hugepage_supported(huge),
                        "unsupported hugepage size: {}",
                        huge);
            }
        }

        let obj_size = if let Some(obj_size) = self.obj_size {
            assert_eq!(obj_size % self.pagesize,
                       0,
                       "object size ({}) is not a multiple of the page size ({})",
                       obj_size,
                       self.pagesize);
            obj_size
        } else {
            self.pagesize
        };
        MapAlloc {
            pagesize: self.pagesize,
            huge_pagesize: self.huge_pagesize,
            perms: perms::get_perm(self.read, self.write, self.exec),
            commit: self.commit,
            obj_size: obj_size,
        }
    }

    #[cfg(target_os = "linux")]
    pub fn default_huge_pagesize(mut self) -> MapAllocBuilder {
        let pagesize = sysconf::page::default_hugepage().expect("huge pages not supported");
        self.pagesize = pagesize;
        self.huge_pagesize = Some(pagesize);
        self
    }

    pub fn huge_pagesize(mut self, pagesize: usize) -> MapAllocBuilder {
        self.huge_pagesize = Some(pagesize);
        self
    }

    /// Enables read permission for allocated memory.
    ///
    /// `read` makes it so that allocated memory will be readable. The default is readable.
    ///
    /// See the "Memory Permissions" section of the `MapAllocBuilder` documentation for more
    /// details.
    pub fn read(mut self) -> MapAllocBuilder {
        self.read = true;
        self
    }

    /// Enables write permission for allocated memory.
    ///
    /// `write` makes it so that allocated memory will be writable. The default is writable.
    ///
    /// See the "Memory Permissions" section of the `MapAllocBuilder` documentation for more
    /// details.
    pub fn write(mut self) -> MapAllocBuilder {
        self.write = true;
        self
    }

    /// Enables execution permission for allocated memory.
    ///
    /// `exec` makes it so that allocated memory will be executable. The default is non-executable.
    ///
    /// See the "Memory Permissions" section of the `MapAllocBuilder` documentation for more
    /// details.
    pub fn exec(mut self) -> MapAllocBuilder {
        self.exec = true;
        self
    }

    /// Disables read permission for allocated memory.
    ///
    /// `no_read` makes it so that allocated memory will not be readable. The default is readable.
    ///
    /// See the "Memory Permissions" section of the `MapAllocBuilder` documentation for more
    /// details.
    pub fn no_read(mut self) -> MapAllocBuilder {
        self.read = false;
        self
    }

    /// Disables write permission for allocated memory.
    ///
    /// `no_write` makes it so that allocated memory will not be writable. The default is writable.
    ///
    /// See the "Memory Permissions" section of the `MapAllocBuilder` documentation for more
    /// details.
    pub fn no_write(mut self) -> MapAllocBuilder {
        self.write = false;
        self
    }

    /// Disables execution permission for allocated memory.
    ///
    /// `no_exec` makes it so that allocated memory will not be executable. The default is
    /// non-executable.
    ///
    /// See the "Memory Permissions" section of the `MapAllocBuilder` documentation for more
    /// details.
    pub fn no_exec(mut self) -> MapAllocBuilder {
        self.exec = false;
        self
    }

    /// Makes it so that `alloc` returns committed memory.
    ///
    /// `commit` makes it so that the memory returned by `alloc` is already in a committed state.
    /// The default is to have memory be returned by `alloc` uncommitted.
    ///
    /// # Platform-specific behavior
    ///
    /// `commit` is only supported on Linux and Windows.
    #[cfg(any(target_os = "linux", windows))]
    pub fn commit(mut self) -> MapAllocBuilder {
        self.commit = true;
        self
    }

    /// Makes it so that `alloc` returns uncommitted memory.
    ///
    /// `no_commit` makes it so that the memory returned by `alloc` is in an uncommitted state.
    /// This is the default.
    ///
    /// # Platform-specific behavior
    ///
    /// `no_commit` is only supported on Linux and Windows.
    #[cfg(any(target_os = "linux", windows))]
    pub fn no_commit(mut self) -> MapAllocBuilder {
        self.commit = false;
        self
    }

    /// Sets the object size for the `UntypedObjectAlloc` implementation.
    ///
    /// `MapAlloc` implements `UntypedObjectAlloc`. `obj_size` sets the object size that will be
    /// used by that implementation. It defaults to whatever page size is configured for the
    /// allocator.
    pub fn obj_size(mut self, obj_size: usize) -> MapAllocBuilder {
        self.obj_size = Some(obj_size);
        self
    }
}

impl Default for MapAllocBuilder {
    fn default() -> MapAllocBuilder {
        MapAllocBuilder {
            read: true,
            write: true,
            exec: false,
            commit: false,
            pagesize: sysconf::page::pagesize(),
            huge_pagesize: None,
            obj_size: None,
        }
    }
}

pub struct MapAlloc {
    pagesize: usize,
    huge_pagesize: Option<usize>,
    perms: perms::Perm,
    commit: bool,
    obj_size: usize,
}

impl Default for MapAlloc {
    fn default() -> MapAlloc {
        MapAllocBuilder::default().build()
    }
}

impl MapAlloc {
    // alloc_helper performs the requested allocation, properly handling the case in which map
    // returns null.
    unsafe fn alloc_helper(&self, size: usize) -> Option<*mut u8> {
        // Since allocators in Rust are not allowed to return null pointers, but it is valid for
        // mmap and VirtualFree to return memory starting at null, we have to handle that case. We
        // do this by checking for null, and if we find that map has returned null, we unmap all
        // but the first page and try again. Since we leave the first page (the one starting at
        // address 0) mapped, future calls to map are guaranteed to not return null. Note that this
        // leaks memory since we never unmap that page, but this isn't a big deal - even if the
        // page is a huge page, since we never write to it, it will remain uncommitted and will
        // thus not consume any physical memory.
        #[cold]
        unsafe fn fix_null(allocator: &MapAlloc, size: usize) -> Option<*mut u8> {
            let unmap_size = size - allocator.pagesize;
            if unmap_size > 0 {
                unmap(allocator.pagesize as *mut u8, unmap_size);
            }
            // a) Make it more likely that the kernel will not keep the page backed by physical
            // memory and, b) make it so that an access to that range will result in a segfault to
            // make other bugs easier to detect.
            #[cfg(any(target_os = "linux", target_os = "macos"))]
            mark_unused(ptr::null_mut(), allocator.pagesize);
            allocator.alloc_helper(size)

        }
        // NOTE: self.commit is guaranteed to be false on Mac.
        map(size, self.perms, self.commit, self.huge_pagesize).and_then(|ptr| {
            if ptr.is_null() {
                fix_null(self, size)
            } else {
                Some(ptr)
            }
        })
    }

    #[cfg(target_os = "linux")]
    unsafe fn realloc_helper(&self, old_ptr: *mut u8, old_size: usize, new_size: usize) -> Option<*mut u8> {
        // Since allocators in Rust are not allowed to return null pointers, but it may be valid for
        // mremap to return memory starting at null, we have to handle that case. We
        // do this by checking for null, and if we find that map has returned null, we create a
        // new mapping, and mremap all pages after the first into that region. We store the first
        // byte of the original map (to avoid trying to read data at NULL,
        // which is Undefined Behaviour for LLVM), and set that byte in
        // the new mapping. Then memcpy the remainder of the page at NULL.
        // Finally, we tell the OS that the page at NULL is unused, but do not unmap it.
        // Since we leave the page at 0 mapped, future calls should never return null.
        // Note that this leaks memory since we never unmap that page, but this isn't a big deal -
        // since we never write to it, it will remain uncommitted and will thus not consume any
        // physical memory.
        #[cold]
        unsafe fn fix_null(allocator: &MapAlloc, size: usize, first_byte: u8) ->  *mut u8 {
            use core::cmp;

            // First create a mapping that will serve as the destination of the remap
            let new_ptr = map(size, perms::PROT_WRITE, false, allocator.huge_pagesize)
                .expect("Not enough virtual memory to make new mapping");
            debug_assert!(!new_ptr.is_null(),
                          "we have an open mapping on null, new mapping should never be null");

            // remap onto the newly-created mapping. This should never fail because the target
            // mapping already exists.
            let move_size = size - allocator.pagesize;
            if move_size > 0 {
                let move_result = libc::mremap(
                    allocator.pagesize as *mut _,
                    move_size,
                    move_size,
                    libc::MREMAP_MAYMOVE | libc::MREMAP_FIXED,
                    new_ptr.offset(allocator.pagesize as isize)
                );
                // Should never fail: we're remapping into an existing mapping
                assert_ne!(move_result, libc::MAP_FAILED, "Unable to move mapping: {}", errno());
            }

            *new_ptr = first_byte;
            ptr::copy_nonoverlapping(
                1 as *const _,
                new_ptr.offset(1),
                cmp::min(allocator.pagesize - 1, size - 1)
            );
            // a) Make it more likely that the kernel will not keep the page backed by physical
            // memory and, b) make it so that an access to that range will result in a segfault to
            // make other bugs easier to detect.
            mark_unused(ptr::null_mut(), allocator.pagesize);
            new_ptr
        }

        // in case of a null mapping, we need to be able read the first byte
        if self.perms & perms::PROT_READ == 0 {
            libc::mprotect(old_ptr as *mut _, self.pagesize, self.perms | perms::PROT_READ);
        }
        let first_byte = *old_ptr;
        remap(old_ptr, old_size, new_size, false).map(|ptr| {
            let new_ptr = if ptr.is_null() {
                fix_null(self, new_size, first_byte)
            } else {
                ptr
            };
            // if we got a null mapping, the first page was marked writable, so
            // reset the permissions on the first page, even if we didn't have to change it to make
            // it readable
            if ptr.is_null() || self.perms & perms::PROT_READ == 0 {
                libc::mprotect(new_ptr as *mut _, self.pagesize, self.perms);
            }
            new_ptr
        })
    }

    /// Commits an existing allocated object.
    ///
    /// `commit` moves the given object into the committed state. After `commit` has returned, the
    /// object can be accessed without crashing the program. If this `MapAlloc` was configured with
    /// the "commit" option disabled (the default), then allocated objects are unusable until they
    /// have been committed by this method.
    ///
    /// If `commit` is called on an already-committed object, it does nothing.
    ///
    /// # Platform-specific behavior
    ///
    /// `commit` is only present on Windows. On Linux and Mac, uncommitted memory is automatically
    /// committed upon the first access.
    #[cfg(windows)]
    pub unsafe fn commit(&self, ptr: *mut u8, layout: Layout) {
        // TODO: What to do about sizes that are not multiples of the page size? These are legal
        // allocations, and so they are legal to pass to uncommit, but will VirtualFree handle them
        // properly?
        debug_assert!(layout.size() > 0, "commit: size of layout must bigger than 0");
        #[cfg(debug_assertions)]
        self.debug_verify_ptr(ptr, layout.clone());
        commit(ptr, layout.size(), self.perms);
    }

    /// Uncommits an existing allocated object.
    ///
    /// `uncommit` moves the given object into the uncommitted state. If `uncommit` is called on
    /// an already-uncommitted object, it does nothing.
    ///
    /// # Platform-specific behavior
    ///
    /// On Windows, after `uncommit` has returned, the object cannot be accessed without crashing
    /// the program. On Linux and Mac, the memory can still safely be accessed, but it may have
    /// been zeroed.
    #[cfg(any(target_os = "linux", target_os = "macos", windows))]
    pub unsafe fn uncommit(&self, ptr: *mut u8, layout: Layout) {
        // TODO: What to do about sizes that are not multiples of the page size? These are legal
        // allocations, and so they are legal to pass to uncommit, but will madvise handle them
        // properly?
        debug_assert!(layout.size() > 0, "uncommit: size of layout must bigger than 0");
        #[cfg(debug_assertions)]
        self.debug_verify_ptr(ptr, layout.clone());
        uncommit(ptr, layout.size());
    }

    fn debug_verify_ptr(&self, ptr: *mut u8, layout: Layout) {
        if let Some(huge) = self.huge_pagesize {
            debug_assert_eq!(ptr as usize % huge,
                             0,
                             "ptr {:?} not aligned to huge page size {}",
                             ptr,
                             huge);
            debug_assert!(layout.align() <= huge);
        } else {
            debug_assert_eq!(ptr as usize % self.pagesize,
                             0,
                             "ptr {:?} not aligned to page size {}",
                             ptr,
                             self.pagesize);
            debug_assert!(layout.align() <= self.pagesize);
        }
    }
}

unsafe impl<'a> Alloc for &'a MapAlloc {
    unsafe fn alloc(&mut self, layout: Layout) -> Result<*mut u8, AllocErr> {
        // alignment less than a page is fine because page-aligned objects are also aligned to
        // any alignment less than a page
        debug_assert!(layout.size() > 0, "alloc: size of layout must bigger than 0");
        if layout.align() > self.pagesize {
            return Err(AllocErr::invalid_input("cannot support alignment greater than a page"));
        }

        let size = next_multiple(layout.size(), self.pagesize);
        self.alloc_helper(size).ok_or(AllocErr::Exhausted { request: layout })
    }

    unsafe fn dealloc(&mut self, ptr: *mut u8, layout: Layout) {
        debug_assert!(layout.size() > 0, "dealloc: size of layout must bigger than 0");
        unmap(ptr, layout.size());
    }

    fn usable_size(&self, layout: &Layout) -> (usize, usize) {
        debug_assert!(layout.size() > 0, "usable_size: size of layout must bigger than 0");
        let max_size = next_multiple(layout.size(), self.pagesize);
        (max_size - self.pagesize + 1, max_size)
    }

    unsafe fn alloc_zeroed(&mut self, layout: Layout) -> Result<*mut u8, AllocErr> {
        debug_assert!(layout.size() > 0, "alloc_zeroed: size of layout must bigger than 0");
        <&'a MapAlloc as Alloc>::alloc(self, layout)
    }

    #[cfg(target_os = "linux")]
    unsafe fn realloc(&mut self, ptr: *mut u8, layout: Layout, new_layout: Layout) -> Result<*mut u8, AllocErr> {
        // alignment less than a page is fine because page-aligned objects are also aligned to
        // any alignment less than a page
        if new_layout.align() > self.pagesize {
            return Err(AllocErr::invalid_input("cannot support alignment greater than a page"));
        }
        debug_assert!(layout.size() > 0 && new_layout.size() > 0,
            "usable_size: size of layout and new_layout must bigger than 0");

        let old_size = next_multiple(layout.size(), self.pagesize);
        let new_size = next_multiple(new_layout.size(), self.pagesize);
        if old_size == new_size {
            return Ok(ptr);
        }
        self.realloc_helper(ptr, old_size, new_layout.size()).ok_or(AllocErr::Exhausted { request: new_layout })
    }

    #[cfg(target_os = "linux")]
    unsafe fn grow_in_place(&mut self, ptr: *mut u8, layout: Layout, new_layout: Layout) -> Result<(), CannotReallocInPlace> {
        // alignment less than a page is fine because page-aligned objects are also aligned to
        // any alignment less than a page
        if new_layout.align() > self.pagesize {
            return Err(CannotReallocInPlace);
        }
        debug_assert!(layout.size() > 0 && new_layout.size() > 0,
            "grow_in_place: size of layout and new_layout must bigger than 0");

        let old_size = next_multiple(layout.size(), self.pagesize);
        let new_size = next_multiple(new_layout.size(), self.pagesize);
        if old_size == new_size {
            return Ok(());
        }
        match remap(ptr, old_size, new_size, true) {
            Some(new_ptr) => {
                debug_assert_eq!(new_ptr, ptr);
                Ok(())
            }
            None => Err(CannotReallocInPlace),
        }
    }

    #[cfg(target_os = "linux")]
    unsafe fn shrink_in_place(&mut self, ptr: *mut u8, layout: Layout, new_layout: Layout) -> Result<(), CannotReallocInPlace> {
        // grow in place does not check if new_layout is larger than the old layout.
        self.grow_in_place(ptr, layout, new_layout)
    }
}

unsafe impl<'a> UntypedObjectAlloc for &'a MapAlloc {
    fn layout(&self) -> Layout {
        if cfg!(debug_assertions) {
            Layout::from_size_align(self.obj_size, self.pagesize).unwrap()
        } else {
            unsafe { Layout::from_size_align_unchecked(self.obj_size, self.pagesize) }
        }
    }

    unsafe fn alloc(&mut self) -> Result<*mut u8, Exhausted> {
        // TODO: There's probably a method that does this more cleanly.
        match self.alloc_excess(self.layout()) {
            Ok(Excess(ptr, _)) => Ok(ptr),
            Err(AllocErr::Exhausted { .. }) => Err(Exhausted),
            Err(AllocErr::Unsupported { .. }) => unreachable!(),
        }
    }

    unsafe fn dealloc(&mut self, ptr: *mut u8) {
        unmap(ptr, self.obj_size);
    }
}

unsafe impl Alloc for MapAlloc {
    unsafe fn alloc(&mut self, layout: Layout) -> Result<*mut u8, AllocErr> {
        <&MapAlloc as Alloc>::alloc(&mut (&*self), layout)
    }

    fn usable_size(&self, layout: &Layout) -> (usize, usize) {
        <&MapAlloc as Alloc>::usable_size(&(&*self), layout)
    }

    unsafe fn dealloc(&mut self, ptr: *mut u8, layout: Layout) {
        <&MapAlloc as Alloc>::dealloc(&mut (&*self), ptr, layout)
    }

    unsafe fn alloc_zeroed(&mut self, layout: Layout) -> Result<*mut u8, AllocErr> {
        <&MapAlloc as Alloc>::alloc_zeroed(&mut (&*self), layout)
    }

    unsafe fn alloc_excess(&mut self, layout: Layout) -> Result<Excess, AllocErr> {
        <&MapAlloc as Alloc>::alloc_excess(&mut (&*self), layout)
    }
    
    unsafe fn realloc(&mut self, ptr: *mut u8, layout: Layout, new_layout: Layout) -> Result<*mut u8, AllocErr> {
        <&MapAlloc as Alloc>::realloc(&mut (&*self), ptr, layout, new_layout)
    }
    
    unsafe fn grow_in_place(&mut self, ptr: *mut u8, layout: Layout, new_layout: Layout) -> Result<(), CannotReallocInPlace> {
        <&MapAlloc as Alloc>::grow_in_place(&mut (&*self), ptr, layout, new_layout)
    }

    unsafe fn shrink_in_place(&mut self, ptr: *mut u8, layout: Layout, new_layout: Layout) -> Result<(), CannotReallocInPlace> {
        <&MapAlloc as Alloc>::shrink_in_place(&mut (&*self), ptr, layout, new_layout)
    }
}

unsafe impl UntypedObjectAlloc for MapAlloc {
    fn layout(&self) -> Layout {
        <&MapAlloc as UntypedObjectAlloc>::layout(&(&*self))
    }

    unsafe fn alloc(&mut self) -> Result<*mut u8, Exhausted> {
        <&MapAlloc as UntypedObjectAlloc>::alloc(&mut (&*self))
    }

    unsafe fn dealloc(&mut self, ptr: *mut u8) {
        <&MapAlloc as UntypedObjectAlloc>::dealloc(&mut (&*self), ptr);
    }
}

fn next_multiple(size: usize, unit: usize) -> usize {
    let remainder = size % unit;
    if remainder == 0 {
        size
    } else {
        size + (unit - remainder)
    }
}

#[cfg(target_os = "linux")]
unsafe fn mark_unused(ptr: *mut u8, size: usize) {
    use libc::{c_void, MADV_DONTNEED, PROT_NONE};
    // Let the kernel know we don't need this memory, so it can free physical resources for it
    libc::madvise(ptr as *mut c_void, size, MADV_DONTNEED);
    // Make it so that accesses to this memory result in a segfault
    libc::mprotect(ptr as *mut c_void, size, PROT_NONE);
}

#[cfg(target_os = "macos")]
unsafe fn mark_unused(ptr: *mut u8, size: usize) {
    use libc::{c_void, MADV_FREE, PROT_NONE};
    // Let the kernel know we don't need this memory, so it can free physical resources for it
    libc::madvise(ptr as *mut c_void, size, MADV_FREE);
    // Make it so that accesses to this memory result in a segfault
    libc::mprotect(ptr as *mut c_void, size, PROT_NONE);
}

#[cfg(target_os = "linux")]
unsafe fn map(size: usize,
              perms: i32,
              commit: bool,
              huge_pagesize: Option<usize>)
              -> Option<*mut u8> {
    use libc::{ENOMEM, MAP_ANONYMOUS, MAP_FAILED, MAP_HUGETLB, MAP_PRIVATE, MAP_POPULATE};

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
    } | if commit { MAP_POPULATE } else { 0 };

    let ptr = libc::mmap(ptr::null_mut(),
                         size,
                         perms,
                         MAP_ANONYMOUS | MAP_PRIVATE | flags,
                         -1,
                         0);

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

// commit must be false
#[cfg(target_os = "macos")]
unsafe fn map(size: usize,
              perms: i32,
              commit: bool,
              huge_pagesize: Option<usize>)
              -> Option<*mut u8> {
    use libc::{MAP_ANON, MAP_PRIVATE, MAP_FAILED, ENOMEM};

    debug_assert!(!commit);

    // TODO: Support superpages (see MAP_ANON description in mmap manpage)
    debug_assert!(huge_pagesize.is_none());

    let ptr = libc::mmap(ptr::null_mut(), size, perms, MAP_ANON | MAP_PRIVATE, -1, 0);

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

// type of the size parameter to VirtualAlloc and VirtualFree
#[cfg(all(windows, target_pointer_width = "32"))]
type WindowsSize = u32;
#[cfg(all(windows, target_pointer_width = "64"))]
type WindowsSize = u64;

// For a good overview of virtual memory handling on Windows, see
// https://blogs.technet.microsoft.com/markrussinovich/2008/11/17/pushing-the-limits-of-windows-virtual-memory/

#[cfg(windows)]
unsafe fn map(size: usize,
              perms: u32,
              commit: bool,
              huge_pagesize: Option<usize>)
              -> Option<*mut u8> {
    use kernel32::VirtualAlloc;
    use winapi::winnt::{MEM_RESERVE, MEM_COMMIT, MEM_LARGE_PAGES};

    let typ = MEM_RESERVE | if commit { MEM_COMMIT } else { 0 } |
              if huge_pagesize.is_some() {
                  MEM_LARGE_PAGES
              } else {
                  0
              };

    // NOTE: While Windows makes a distinction between allocation granularity and page size (see
    // https://msdn.microsoft.com/en-us/library/windows/desktop/ms724958(v=vs.85).aspx),
    // VirtualAlloc only cares about allocation granularity for the pointer argument, not the size.
    // Since we're passing null for the pointer, this doesn't affect us.
    let ptr = VirtualAlloc(ptr::null_mut(), size as WindowsSize, typ, perms) as *mut u8;
    // NOTE: Windows can return many different error codes in different scenarios that all relate
    // to being out of memory. Instead of trying to list them all, we assume that any error is an
    // out-of-memory condition. This is fine so long as our code doesn't have a bug (that would,
    // e.g., result in VirtualAlloc being called with invalid arguments). This isn't ideal, but
    // during debugging, error codes can be printed here, so it's not the end of the world.
    if ptr.is_null() { None } else { Some(ptr) }
}

#[cfg(target_os = "linux")]
unsafe fn remap(ptr: *mut u8, old_size: usize, new_size: usize, in_place: bool) -> Option<*mut u8> {
    let flags = if !in_place {
        libc::MREMAP_MAYMOVE
    } else {
        0
    };
    let result = libc::mremap(ptr as *mut _, old_size, new_size, flags);
    if result == libc::MAP_FAILED {
        let err = errno();
        if err.0 == libc::ENOMEM {
            None
        } else {
            panic!("mremap failed: {}", err)
        }
    } else {
        Some(result as *mut u8)
    }
}

#[cfg(any(target_os = "linux", target_os = "macos"))]
unsafe fn unmap(ptr: *mut u8, size: usize) {
    use libc::{munmap, c_void};
    // NOTE: Don't inline the call to munmap; then errno might be called before munmap.
    let ret = munmap(ptr as *mut c_void, size);
    assert_eq!(ret, 0, "munmap failed: {}", errno());
}

#[cfg(windows)]
unsafe fn unmap(ptr: *mut u8, _size: usize) {
    use kernel32::{VirtualFree, GetLastError};
    use winapi::winnt::MEM_RELEASE;

    // NOTE: VirtualFree, when unmapping memory (as opposed to decommitting it), can only operate
    // on an entire region previously mapped with VirtualAlloc. As a result, 'ptr' must have been
    // previously returned by VirtualAlloc, and no length is needed since it is known by the kernel
    // (VirtualFree /requires/ that if the third argument is MEM_RELEASE, the second is 0).
    let ret = VirtualFree(ptr as *mut winapi::c_void, 0, MEM_RELEASE);
    assert_ne!(ret,
               0,
               "Call to VirtualFree failed with error code {}.",
               GetLastError());
}

#[cfg(windows)]
unsafe fn commit(ptr: *mut u8, size: usize, perms: u32) {
    use kernel32::VirtualAlloc;
    use winapi::winnt::MEM_COMMIT;

    let ret = VirtualAlloc(ptr as *mut winapi::c_void,
                           size as WindowsSize,
                           MEM_COMMIT,
                           perms);
    assert_eq!(ret as *mut u8, ptr);
}

#[cfg(target_os = "linux")]
unsafe fn uncommit(ptr: *mut u8, size: usize) {
    use libc::{c_void, MADV_DONTNEED};

    // TODO: Other options such as MADV_FREE are available on newer versions of Linux. Is there
    // a way that we can use those when available? Is that even desirable?
    libc::madvise(ptr as *mut c_void, size, MADV_DONTNEED);
}

#[cfg(target_os = "macos")]
unsafe fn uncommit(ptr: *mut u8, size: usize) {
    use libc::{c_void, MADV_FREE};
    libc::madvise(ptr as *mut c_void, size, MADV_FREE);
}

#[cfg(windows)]
unsafe fn uncommit(ptr: *mut u8, size: usize) {
    use kernel32::{VirtualFree, GetLastError};
    use winapi::winnt::MEM_DECOMMIT;

    let ret = VirtualFree(ptr as *mut winapi::c_void,
                          size as WindowsSize,
                          MEM_DECOMMIT);
    assert_ne!(ret,
               0,
               "Call to VirtualFree failed with error code {}.",
               GetLastError());
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
    use sysconf::page::pagesize;
    use super::*;
    use super::perms::*;


    #[cfg(not(feature = "test-no-std"))]
    extern crate test;

    // allow(unused) because these imports aren't used on windows
    #[allow(unused)]
    #[cfg(not(feature = "test-no-std"))]
    use std::time::{Instant, Duration};
    #[allow(unused)]
    #[cfg(not(feature = "test-no-std"))]
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
    unsafe fn test_zero_filled(ptr: *mut u8, size: usize) {
        for i in 0..size {
            assert_eq!(*ptr.offset(i as isize), 0);
        }
    }

    // Test that the given range is writable.
    unsafe fn test_write(ptr: *mut u8, size: usize) {
        for i in 0..size {
            *ptr.offset(i as isize) = (i & 0xff) as u8;
        }
    }

    // Test that the given range is readable, and matches data written by test_write/test_write_read
    unsafe fn test_read(ptr: *mut u8, size: usize) {
        for i in 0..size {
            assert_eq!(*ptr.offset(i as isize), (i & 0xff) as u8);
        }
    }

    // Test that the given range is readable and writable, and that writes can be read back.
    unsafe fn test_write_read(ptr: *mut u8, size: usize) {
        test_write(ptr, size);
        test_read(ptr, size);
    }

    #[test]
    fn test_small_realloc() {
        unsafe {
            let mut allocator = MapAlloc::default();
            let small = Layout::array::<u8>(1).unwrap();
            let medium = Layout::array::<u8>(2048).unwrap();
            let big = Layout::array::<u8>(4096).unwrap();
            let ptr = <MapAlloc as Alloc>::alloc(&mut allocator, medium.clone()).unwrap();
            test_valid_map_address(ptr);
            test_zero_filled(ptr, medium.size());
            test_write_read(ptr, medium.size());
            allocator.shrink_in_place(ptr, medium.clone(), small.clone()).unwrap();
            test_read(ptr, small.size());
            test_write(ptr, small.size());
            allocator.grow_in_place(ptr, small.clone(), big.clone()).unwrap();
            test_read(ptr, small.size());
            test_write_read(ptr, big.size());
            let old_ptr = ptr;
            let ptr = allocator.realloc(ptr, big.clone(), small.clone()).unwrap();
            assert_eq!(old_ptr, ptr);
            test_read(ptr, small.size());
            test_write_read(ptr, small.size());
            <MapAlloc as Alloc>::dealloc(&mut allocator, ptr, small.clone());
        }
    }

    #[test]
    fn test_large_realloc() {
        unsafe {
            let mut allocator = MapAlloc::default();
            let small = Layout::array::<u8>(allocator.pagesize).unwrap();
            let medium = Layout::array::<u8>(allocator.pagesize * 8).unwrap();
            let big = Layout::array::<u8>(allocator.pagesize * 16).unwrap();
            let mut ptr = <MapAlloc as Alloc>::alloc(&mut allocator, big.clone()).unwrap();
            test_valid_map_address(ptr);
            test_zero_filled(ptr, big.size());
            test_write_read(ptr, big.size());
            if cfg!(target_os = "linux") {
                allocator.shrink_in_place(ptr, big.clone(), small.clone()).unwrap();
                test_read(ptr, small.size());
                test_write(ptr, small.size());
                allocator.grow_in_place(ptr, small.clone(), medium.clone()).unwrap();
                test_read(ptr, small.size());
                test_write_read(ptr, medium.size());
            } else {
                ptr = allocator.realloc(ptr, big.clone(), medium.clone()).unwrap();
                test_valid_map_address(ptr);
                test_read(ptr, medium.size());
                test_read(ptr, medium.size());
            }
            let old_ptr = ptr;
            let ptr = allocator.realloc(ptr, medium.clone(), small.clone()).unwrap();
            if cfg!(target_os = "linux") {
                assert_eq!(old_ptr, ptr);
            }
            test_read(ptr, small.size());
            test_write(ptr, small.size());
            let ptr = allocator.realloc(ptr, small.clone(), big.clone()).unwrap();
            if cfg!(target_os = "linux") {
                assert_eq!(old_ptr, ptr);
            }
            test_read(ptr, small.size());
            test_write_read(ptr, big.size());

            // Treating the first page as its own allocation, grow that allocation to two pages.
            // Since we know there's a second mapped page right after it, remap will be unable
            // to simply grow the mapping in place, and will be forced to move it.
            let remaining = ptr.offset(allocator.pagesize as isize);
            let remaining_layout = Layout::array::<u8>(big.size() - allocator.pagesize).unwrap();
            let new = allocator.realloc(ptr, small.clone(), medium.clone()).unwrap();
            test_read(new, small.size());
            test_zero_filled(new.offset(small.size() as isize), medium.size() - small.size());
            test_read(remaining, remaining_layout.size());
            <MapAlloc as Alloc>::dealloc(&mut allocator, new, medium.clone());
            <MapAlloc as Alloc>::dealloc(
                &mut allocator,
                remaining,
                remaining_layout.clone()
            );
        }
    }

    #[test]
    fn test_map() {
        unsafe {
            // Check that:
            // - Mapping a single page works
            // - The returned pointer is non-null
            // - The returned pointer is page-aligned
            // - The page is zero-filled (on Windows, after the page is committed)
            // - Unmapping it after it's already been unmapped is OK (except on windows).
            let mut ptr = map(pagesize(), PROT_READ_WRITE, false, None).unwrap();
            test_valid_map_address(ptr);
            #[cfg(windows)]
            commit(ptr, pagesize(), PROT_READ_WRITE);
            test_zero_filled(ptr, pagesize());
            unmap(ptr, pagesize());
            #[cfg(not(windows))]
            unmap(ptr, pagesize());

            // Check that:
            // - Mapping multiple pages work
            // - The returned pointer is non-null
            // - The returned pointer is page-aligned
            // - The pages are zero-filled (on Windows, after the page is committed)
            // - Unmapping it after it's already been unmapped is OK (except on windows).
            ptr = map(16 * pagesize(), PROT_READ_WRITE, false, None).unwrap();
            test_valid_map_address(ptr);
            #[cfg(windows)]
            commit(ptr, 16 * pagesize(), PROT_READ_WRITE);
            test_zero_filled(ptr, 16 * pagesize());
            unmap(ptr, 16 * pagesize());
            #[cfg(not(windows))]
            unmap(ptr, 16 * pagesize());
        }
    }

    #[cfg(not(windows))]
    #[test]
    fn test_map_non_windows() {
        unsafe {
            // Check that:
            // - Unmapping a subset of a previously-mapped region works
            // - The remaining pages are still mapped
            let mut ptr = map(5 * pagesize(), PROT_READ_WRITE, false, None).unwrap();
            test_valid_map_address(ptr);
            test_zero_filled(ptr, 5 * pagesize());
            unmap(ptr, pagesize());
            unmap(ptr.offset(2 * pagesize() as isize), pagesize());
            unmap(ptr.offset(4 * pagesize() as isize), pagesize());
            test_zero_filled(ptr.offset(1 * pagesize() as isize), pagesize());
            test_zero_filled(ptr.offset(3 * pagesize() as isize), pagesize());

            // Check that:
            // - Mapping a vast region of memory works and is fast
            // - The returned pointer is non-null
            // - The returned pointer is page-aligned
            // - A read in the middle of mapping succeds and is zero

            // NOTE: Pick 2^29 bytes because, on Linux, 2^30 causes map to return null, which breaks
            // test_valid_map_address.
            let size = 1 << 29;
            #[cfg(not(feature = "test-no-std"))]
            let t0 = Instant::now();
            ptr = map(size, PROT_READ_WRITE, false, None).unwrap();
            #[cfg(not(feature = "test-no-std"))]
            {
                // In tests on a 2016 MacBook Pro (see bench_large_map), a 2^31 byte map/unmap pair
                // took ~5 usec natively (Mac OS X) and ~350 ns in a Linux VM. Thus, 1 ms is a safe
                // upper bound.
                let diff = Instant::now().duration_since(t0);
                let target = Duration::from_millis(1);
                assert!(diff < target, "duration: {:?}", diff);
            }
            test_valid_map_address(ptr);
            test_zero_filled(ptr.offset((size / 2) as isize), pagesize());
            unmap(ptr, size);
        }
    }

    #[test]
    fn test_commit() {
        unsafe {
            // Check that:
            // - Mapping and committing a single page works (except on Mac, which doesn't support
            //   committing)
            // - The returned pointer is non-null
            // - The returned pointer is page-aligned
            // - We can read that page, and it is zero-filled (on Unix, this test is trivial, but
            //   on Windows, it ensures that map properly committed the page)
            let mut ptr = map(pagesize(),
                              PROT_READ_WRITE,
                              !cfg!(target_os = "macos"),
                              None)
                    .unwrap();
            test_valid_map_address(ptr);
            test_zero_filled(ptr, pagesize());
            unmap(ptr, pagesize());

            // Check that:
            // - Mapping a single page works
            // - The returned pointer is non-null
            // - The returned pointer is page-aligned
            // - We can read that page, and it is zero-filled (on Windows, after the page is committed)
            // - On Windows, we can commit the page after it has already been committed
            // - We can uncommit the page
            // - We can uncommit the page after it has already been uncommitted
            ptr = map(pagesize(), PROT_READ_WRITE, false, None).unwrap();
            test_valid_map_address(ptr);
            #[cfg(windows)]
            commit(ptr, pagesize(), PROT_READ_WRITE);
            test_zero_filled(ptr, pagesize());
            #[cfg(windows)]
            commit(ptr, pagesize(), PROT_READ_WRITE);
            uncommit(ptr, pagesize());
            uncommit(ptr, pagesize());
            unmap(ptr, pagesize());
        }
    }

    #[test]
    fn test_perms() {
        unsafe {
            // TODO: Add tests for executable permissions

            // Check that:
            // - Mapping a single read-only page works
            // - The returned pointer is non-null
            // - The returned pointer is page-aligned
            // - We can read the page, and it is zero-filled (on Windows, after the page is committed)
            let mut ptr = map(pagesize(), PROT_READ, false, None).unwrap();
            test_valid_map_address(ptr);
            #[cfg(windows)]
            commit(ptr, pagesize(), PROT_READ);
            test_zero_filled(ptr, pagesize());
            unmap(ptr, pagesize());

            // Check that:
            // - Mapping a single write-only page works
            // - The returned pointer is non-null
            // - The returned pointer is page-aligned
            // - We can write to the page (on Windows, after the page is committed)
            ptr = map(pagesize(), PROT_WRITE, false, None).unwrap();
            test_valid_map_address(ptr);
            #[cfg(windows)]
            commit(ptr, pagesize(), PROT_WRITE);
            test_write(ptr, pagesize());
            unmap(ptr, pagesize());

            // Check that:
            // - Mapping a single read-write page works
            // - The returned pointer is non-null
            // - The returned pointer is page-aligned
            // - We can read the page, and it is zero-filled (on Windows, after the page is committed)
            // - We can write to the page, and those writes are properly read back
            ptr = map(pagesize(), PROT_READ_WRITE, false, None).unwrap();
            test_valid_map_address(ptr);
            #[cfg(windows)]
            commit(ptr, pagesize(), PROT_READ_WRITE);
            test_zero_filled(ptr, pagesize());
            test_write_read(ptr, pagesize());
            unmap(ptr, pagesize());
        }
    }

    #[cfg(not(windows))]
    #[test]
    #[should_panic]
    fn test_map_panic_zero() {
        unsafe {
            // Check that zero length causes map to panic. On Windows, our map implementation never
            // panics.
            map(0, PROT_READ_WRITE, false, None);
        }
    }

    #[cfg(all(not(all(target_os = "linux", target_pointer_width = "64")), not(windows)))]
    #[test]
    #[should_panic]
    fn test_map_panic_too_large() {
        unsafe {
            // Check that an overly large length causes map to panic. On Windows, our map
            // implementation never panics. On 64-bit Linux, map simply responds to overly large maps
            // by returning ENOMEM.
            use core::usize::MAX;
            map(MAX, PROT_READ_WRITE, false, None);
        }
    }

    #[cfg(not(windows))]
    #[test]
    #[should_panic]
    fn test_unmap_panic_zero() {
        unsafe {
            // Check that zero length causes unmap to panic. On Windows, the length parameter is
            // ignored, so the page will simply be unmapped normally.

            // NOTE: This test leaks memory, but it's only a page, so it doesn't really matter.
            let ptr = map(pagesize(), PROT_READ_WRITE, false, None).unwrap();
            unmap(ptr, 0);
        }
    }

    #[test]
    #[should_panic]
    fn test_unmap_panic_unaligned() {
        unsafe {
            // Check that a non-page-aligned address causes unmap to panic.
            unmap((pagesize() / 2) as *mut u8, pagesize());
        }
    }

    #[cfg(not(windows))]
    #[cfg(not(feature = "test-no-std"))]
    #[bench]
    #[ignore]
    fn bench_large_map(b: &mut Bencher) {
        // Determine the speed of mapping a large region of memory so that we can tune the timeout
        // in test_map_non_windows.
        b.iter(|| unsafe {
                   let ptr = map(1 << 29, PROT_READ_WRITE, false, None).unwrap();
                   unmap(ptr, 1 << 29);
               })
    }
}
