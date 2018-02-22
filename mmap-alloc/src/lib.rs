// Copyright 2017-2018 the authors. See the 'Copyright and license' section of the
// README.md file at the top-level directory of this repository.
//
// Licensed under the Apache License, Version 2.0 (the LICENSE-APACHE file) or
// the MIT license (the LICENSE-MIT file) at your option. This file may not be
// copied, modified, or distributed except according to those terms.

// TODO:
// - Figure out how to panic without allocating
// - Support all Unices, not just Linux and Mac
// - Add tests for UntypedObjectAlloc impls

#![cfg_attr(not(test), no_std)]
#![cfg_attr(test, feature(test))]
#![feature(alloc, allocator_api)]

#[cfg(not(any(target_os = "linux", target_os = "macos", windows)))]
compile_error!("mmap-alloc only supports Windows, Linux, and Mac");

#[cfg(test)]
mod tests;

#[cfg(test)]
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

use self::alloc::allocator::{Alloc, AllocErr, CannotReallocInPlace, Excess, Layout};
use self::object_alloc::{Exhausted, UntypedObjectAlloc};
use core::ptr::{self, NonNull};

#[cfg(any(target_os = "linux", target_os = "macos"))]
use errno::errno;

/// A builder for `MapAlloc`.
///
/// `MapAllocBuilder` represents the configuration of a `MapAlloc`. New `MapAllocBuilder`s are
/// constructed using `default`, and then various other methods are used to set various
/// configuration options.
///
/// # Committed and Uncommitted Memory
///
/// Memory pages allocated by the kernel are [virtual memory pages][virtual memory]. Virtual
/// memory pages can be in two states - committed or uncommitted. When in the uncommitted state,
/// there is not necessarily any physical memory or swap space on disk associated with the page.
/// When in the committed state, there is. When a page is allocated, it starts off in the
/// uncommitted state (although the `commit` method can be used to configure this behavior).
/// When an uncommitted page is committed, a physical memory page or region of swap space is
/// allocated and associated with it. This memory has its bytes initialized to zero. When a
/// committed page is uncommitted, the kernel is free to reclaim these physical resources, and
/// in doing so, may wipe away the contents of the page (such that, if the page is later committed
/// again, its contents will have been set back to zero).
///
/// On Linux and Mac, if an uncommitted page is accessed (read, written, or executed), it is
/// automatically committed by the kernel. Thus, while explicitly committing memory may improve
/// performance, it does not change the behavior of the program in any observable way.
///
/// On Windows, if an uncommitted page is accessed, it will cause a fault. Thus, Windows memory
/// must be explicitly committed before use, and the uncommitted/committed distinction affects
/// the observable behavior of the program.
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
///
/// # Alignment
///
/// Since memory is allocated by mapping pages directly from the kernel, all allocations are
/// automatically aligned to the size of a memory page. Since there is no way to request a
/// higher alignment from the kernel, higher alignments are not supported by `MapAlloc` by default.
///
/// However, this crate provides the `large-align` feature (disabled by default) which works around
/// this limitation to allow for arbitrarily large alignments. When an alignment larger than the page
/// size is requested, `alignment - page size` extra bytes are allocated. There is guaranteed to be
/// a properly-aligned chunk of memory of the requested size somewhere within this larger allocation.
/// That chunk is located and returned. The `large-align` feature also provides an `object_align`
/// method on `MapAllocBuilder` that configures the alignment of the `UntypedObjectAlloc` implementation.
///
/// ## Platform-specific behavior
///
/// On Linux and Mac, all extra leading and trailing memory surrounding the located chunk is
/// deallocated, leaving only the requested object allocated. On Windows, that extra memory is left
/// allocated, but is left in the uncommitted state. This should ensure that it does not consume any
/// physical resources, although it will still consume virtual memory space, which may be scarce,
/// especially on 32-bit platforms.
///
/// ## Limitations
///
/// Currently, the `large-align` feature does not support large alignments in the `realloc` method,
/// although it is fine to pass an object which was previously allocated with a large alignment
/// as the object to be reallocated (it may have a smaller alignment after being reallocated).
///
/// [virtual memory]: https://en.wikipedia.org/wiki/Virtual_memory
pub struct MapAllocBuilder {
    read: bool,
    write: bool,
    exec: bool,
    // Only supported on Linux (which has MAP_POPULATE) and Windows (which has MEM_COMMIT)
    commit: bool,
    // sysconf::page::pagesize might be inefficient, so store a copy of the pagesize to ensure that
    // loading it is efficient
    pagesize: usize,
    // used for the UntypedObjectAlloc implementation
    obj_size: Option<usize>,
    obj_align: Option<usize>,
}

impl MapAllocBuilder {
    pub fn build(&self) -> MapAlloc {
        let obj_size = if let Some(obj_size) = self.obj_size {
            assert_eq!(
                obj_size % self.pagesize,
                0,
                "object size ({}) is not a multiple of the page size ({})",
                obj_size,
                self.pagesize
            );
            obj_size
        } else {
            self.pagesize
        };
        let obj_align = if let Some(obj_align) = self.obj_align {
            assert_eq!(
                obj_size % obj_align,
                0,
                "object size ({}) is not a multiple of the object alignment ({})",
                obj_size,
                obj_align,
            );
            obj_align
        } else {
            self.pagesize
        };

        MapAlloc {
            pagesize: self.pagesize,
            read: self.read,
            write: self.write,
            exec: self.exec,
            perms: perms::get_perm(self.read, self.write, self.exec),
            commit: self.commit,
            obj_layout: Layout::from_size_align(obj_size, obj_align).unwrap(),
        }
    }

    /// Configures read permission for allocated memory.
    ///
    /// `read` configures whether or not allocated memory will be readable. The default is
    /// readable.
    ///
    /// See the "Memory Permissions" section of the `MapAllocBuilder` documentation for more
    /// details.
    pub fn read(mut self, read: bool) -> MapAllocBuilder {
        self.read = read;
        self
    }

    /// Configures write permission for allocated memory.
    ///
    /// `write` configures whether or not allocated memory will be writable. The default is
    /// writable.
    ///
    /// See the "Memory Permissions" section of the `MapAllocBuilder` documentation for more
    /// details.
    pub fn write(mut self, write: bool) -> MapAllocBuilder {
        self.write = write;
        self
    }

    /// Configures execute permission for allocated memory.
    ///
    /// `exec` configures whether or not allocated memory will be executable. The default is
    /// non-executable.
    ///
    /// See the "Memory Permissions" section of the `MapAllocBuilder` documentation for more
    /// details.
    pub fn exec(mut self, exec: bool) -> MapAllocBuilder {
        self.exec = exec;
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

    /// Configures whether `alloc` returns committed memory.
    ///
    /// `commit` configures whether the memory returned by `alloc` is already in a committed state.
    /// The default is to have memory be returned by `alloc` uncommitted.
    ///
    /// See the "Committed and Uncommitted Memory" section of the `MapAllocBuilder` documentation
    /// for more details.
    pub fn commit(mut self, commit: bool) -> MapAllocBuilder {
        self.commit = commit;
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

    /// Sets the object alignment for the `UntypedObjectAlloc` implementation.
    ///
    /// See the "Alignment" section of the `MapAllocBuilder` documentation for more details.
    ///
    /// This method is only available when the `large-align` feature is enabled.
    #[cfg(feature = "large-align")]
    pub fn obj_align(mut self, obj_align: usize) -> MapAllocBuilder {
        self.obj_align = Some(obj_align);
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
            obj_size: None,
            obj_align: None,
        }
    }
}

pub struct MapAlloc {
    // sysconf::page::pagesize might be inefficient, so store a copy of the pagesize to ensure that
    // loading it is efficient
    pagesize: usize,
    #[cfg_attr(target_os = "linux", allow(unused))]
    read: bool,
    #[cfg_attr(target_os = "linux", allow(unused))]
    write: bool,
    #[cfg_attr(target_os = "linux", allow(unused))]
    exec: bool,
    perms: perms::Perm,
    commit: bool,
    // used in UntypedObjectAlloc implementation
    obj_layout: Layout,
}

impl Default for MapAlloc {
    fn default() -> MapAlloc {
        MapAllocBuilder::default().build()
    }
}

impl MapAlloc {
    /// Commits an existing allocated object.
    ///
    /// `commit` moves the given object into the committed state. If `commit` is called on an
    /// already-committed object, it does nothing.
    ///
    /// See the "Committed and Uncommitted Memory" section of the `MapAllocBuilder` documentation
    /// for more details.
    #[cfg_attr(feature = "cargo-clippy", allow(needless_pass_by_value))]
    pub unsafe fn commit(&self, ptr: *mut u8, layout: Layout) {
        debug_assert!(layout.size() > 0, "commit: size of layout must be non-zero");

        // TODO: What to do about sizes that are not multiples of the page size? These are legal
        // allocations, and so they are legal to pass to uncommit, but will madvise/VirtualFree
        // handle them properly?
        #[cfg(debug_assertions)]
        self.debug_verify_ptr(ptr, &layout);
        #[cfg(any(target_os = "linux", target_os = "macos"))]
        commit(ptr, layout.size());
        #[cfg(windows)]
        commit(ptr, layout.size(), self.perms);
    }

    /// Uncommits an existing allocated object.
    ///
    /// `uncommit` moves the given object into the uncommitted state. If `uncommit` is called on
    /// an already-uncommitted object, it does nothing.
    ///
    /// See the "Committed and Uncommitted Memory" section of the `MapAllocBuilder` documentation
    /// for more details.
    #[cfg_attr(feature = "cargo-clippy", allow(needless_pass_by_value))]
    pub unsafe fn uncommit(&self, ptr: *mut u8, layout: Layout) {
        debug_assert!(
            layout.size() > 0,
            "uncommit: size of layout must be non-zero"
        );

        // TODO: What to do about sizes that are not multiples of the page size? These are legal
        // allocations, and so they are legal to pass to uncommit, but will madvise handle them
        // properly?
        #[cfg(debug_assertions)]
        self.debug_verify_ptr(ptr, &layout);
        uncommit(ptr, layout.size());
    }

    #[cfg(target_os = "linux")]
    unsafe fn resize_in_place(
        &self,
        ptr: *mut u8,
        layout: &Layout,
        new_layout: &Layout,
    ) -> Result<(), CannotReallocInPlace> {
        // alignment less than a page is fine because page-aligned objects are also aligned to
        // any alignment less than a page
        if new_layout.align() > self.pagesize {
            return Err(CannotReallocInPlace);
        }

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

    #[cfg(debug_assertions)]
    fn debug_verify_ptr(&self, ptr: *mut u8, layout: &Layout) {
        debug_assert_eq!(
            ptr as usize % self.pagesize,
            0,
            "ptr {:?} not aligned to page size {}",
            ptr,
            self.pagesize
        );
        debug_assert!(layout.align() <= self.pagesize);
    }
}

unsafe impl<'a> Alloc for &'a MapAlloc {
    unsafe fn alloc(&mut self, layout: Layout) -> Result<*mut u8, AllocErr> {
        debug_assert!(layout.size() > 0, "alloc: size of layout must be non-zero");

        let size = next_multiple(layout.size(), self.pagesize);

        // alignment less than a page is fine because page-aligned objects are also aligned to
        // any alignment less than a page
        if layout.align() <= self.pagesize {
            map(size, self.perms, self.commit).ok_or(AllocErr::Exhausted { request: layout })
        } else if cfg!(feature = "large-align") {
            let extra = layout.align() - self.pagesize;
            // Since we're allocating extra space, we don't commit this memory.
            // We only commit the portion we're actually returning below.
            let addr = map(size + extra, self.perms, false).ok_or(AllocErr::Exhausted {
                request: layout.clone(),
            })? as usize;
            let aligned_addr = next_multiple(addr, layout.align());
            let aligned_ptr = aligned_addr as *mut u8;
            if !cfg!(windows) {
                // We only do this on Linux and Mac because on Windows, you can only
                // unmap entire regions allocated with VirtualAlloc. Thus, on Windows,
                // the best we can do is to leave the unused portions of the allocation
                // unmapped. This consumes extra virtual address space, which may be
                // problematic on 32-bit platforms or when virtual address space is
                // scarce for some other reason.
                let prefix_size = aligned_addr - addr;
                let suffix_size = extra - prefix_size;
                if prefix_size > 0 {
                    unmap(addr as *mut u8, prefix_size);
                }
                if suffix_size > 0 {
                    unmap((aligned_addr + size) as *mut u8, suffix_size);
                }
            }
            if self.commit {
                #[cfg(any(target_os = "linux", target_os = "macos"))]
                commit(aligned_ptr, layout.size());
                #[cfg(windows)]
                commit(aligned_ptr, layout.size(), self.perms);
            }
            Ok(aligned_ptr)
        } else {
            Err(AllocErr::invalid_input(
                "cannot support alignment greater than a page",
            ))
        }
    }

    unsafe fn dealloc(&mut self, ptr: *mut u8, layout: Layout) {
        debug_assert!(
            layout.size() > 0,
            "dealloc: size of layout must be non-zero"
        );
        #[cfg(all(feature = "large-align", windows))]
        let ptr = {
            use core::mem::{size_of, uninitialized};
            use kernel32::{GetLastError, VirtualQuery};
            use winapi::winnt::MEMORY_BASIC_INFORMATION;

            // Because Windows' VirtualFree function requires its argument
            // to be a value previously returned from VirtualAlloc, we cannot
            // simply pass ptr, as the large-align feature means ptr might
            // not be the value we originally allocated with VirtualAlloc.
            // We use VirtualQuery here to recover the original value
            // returned from VirtualAlloc.
            let mut info: MEMORY_BASIC_INFORMATION = uninitialized();
            let info_size = size_of::<MEMORY_BASIC_INFORMATION>() as u64;
            let ret = VirtualQuery(ptr as *mut _, &mut info, info_size);
            assert_eq!(
                ret,
                info_size,
                "Call to VirtualQuery({:?}) failed with error code {}.",
                ptr,
                GetLastError()
            );
            info.AllocationBase as *mut _
        };

        unmap(ptr, layout.size());
    }

    fn usable_size(&self, layout: &Layout) -> (usize, usize) {
        debug_assert!(
            layout.size() > 0,
            "usable_size: size of layout must be non-zero"
        );
        let max_size = next_multiple(layout.size(), self.pagesize);
        (max_size - self.pagesize + 1, max_size)
    }

    unsafe fn alloc_zeroed(&mut self, layout: Layout) -> Result<*mut u8, AllocErr> {
        debug_assert!(
            layout.size() > 0,
            "alloc_zeroed: size of layout must be non-zero"
        );
        <&'a MapAlloc as Alloc>::alloc(self, layout)
    }

    #[cfg(target_os = "linux")]
    unsafe fn realloc(
        &mut self,
        ptr: *mut u8,
        layout: Layout,
        new_layout: Layout,
    ) -> Result<*mut u8, AllocErr> {
        debug_assert!(
            layout.size() > 0,
            "realloc: size of layout must be non-zero"
        );
        debug_assert!(
            new_layout.size() > 0,
            "realloc: size of new_layout must be non-zero"
        );

        // TODO: Handle large-align feature

        // alignment less than a page is fine because page-aligned objects are also aligned to
        // any alignment less than a page
        if new_layout.align() > self.pagesize {
            return Err(AllocErr::invalid_input(
                "cannot support alignment greater than a page",
            ));
        }

        let old_size = next_multiple(layout.size(), self.pagesize);
        let new_size = next_multiple(new_layout.size(), self.pagesize);
        if old_size == new_size {
            return Ok(ptr);
        }
        remap(ptr, old_size, new_layout.size(), false).ok_or(AllocErr::Exhausted {
            request: new_layout,
        })
    }

    #[cfg(any(target_os = "macos", windows))]
    unsafe fn realloc(
        &mut self,
        ptr: *mut u8,
        layout: Layout,
        new_layout: Layout,
    ) -> Result<*mut u8, AllocErr> {
        // Adapted from the Rust standard library.

        debug_assert!(
            layout.size() > 0,
            "realloc: size of layout must be non-zero"
        );
        debug_assert!(
            new_layout.size() > 0,
            "realloc: size of new_layout must be non-zero"
        );

        // TODO: Handle large-align feature

        // alignment less than a page is fine because page-aligned objects are also aligned to
        // any alignment less than a page
        if new_layout.align() > self.pagesize {
            return Err(AllocErr::invalid_input(
                "cannot support alignment greater than a page",
            ));
        }

        let old_size = next_multiple(layout.size(), self.pagesize);
        let new_size = next_multiple(new_layout.size(), self.pagesize);

        if old_size == new_size {
            return Ok(ptr);
        } else if !cfg!(windows) && new_size < old_size {
            // Windows cannot shrink existing mappings.
            if let Ok(()) = self.shrink_in_place(ptr, layout.clone(), new_layout.clone()) {
                return Ok(ptr);
            }
        }

        // otherwise, fall back on alloc + copy + dealloc.
        let result = Alloc::alloc(self, new_layout);
        if let Ok(new_ptr) = result {
            use core::cmp;

            // If we're configured to allocate un-readable or un-writable memory, we need to
            // temporarily mark the old allocation readable and the new allocation writable in
            // order to be able to copy the data.
            let fix_old_perms = !self.read;
            let fix_new_perms = !self.write;
            if fix_old_perms {
                protect(ptr, old_size, perms::get_perm(true, self.write, self.exec));
            }
            if fix_new_perms {
                protect(
                    new_ptr,
                    new_size,
                    perms::get_perm(self.read, true, self.exec),
                );
            }
            ptr::copy_nonoverlapping(ptr as *const u8, new_ptr, cmp::min(old_size, new_size));
            Alloc::dealloc(self, ptr, layout);
            if fix_new_perms {
                protect(new_ptr, new_size, self.perms);
            }
        }
        result
    }

    #[cfg(target_os = "linux")]
    unsafe fn grow_in_place(
        &mut self,
        ptr: *mut u8,
        layout: Layout,
        new_layout: Layout,
    ) -> Result<(), CannotReallocInPlace> {
        debug_assert!(
            layout.size() > 0,
            "grow_in_place: size of layout must be non-zero"
        );
        debug_assert!(
            new_layout.size() > 0,
            "grow_in_place: size of new_layout must be non-zero"
        );
        debug_assert!(new_layout.size() >= layout.size());
        debug_assert_eq!(new_layout.align(), layout.align());

        self.resize_in_place(ptr, &layout, &new_layout)
    }

    #[cfg(target_os = "linux")]
    unsafe fn shrink_in_place(
        &mut self,
        ptr: *mut u8,
        layout: Layout,
        new_layout: Layout,
    ) -> Result<(), CannotReallocInPlace> {
        debug_assert!(
            layout.size() > 0,
            "shrink_in_place: size of layout must be non-zero"
        );
        debug_assert!(
            new_layout.size() > 0,
            "shrink_in_place: size of new_layout must be non-zero"
        );
        debug_assert!(new_layout.size() <= layout.size());
        debug_assert_eq!(new_layout.align(), layout.align());

        self.resize_in_place(ptr, &layout, &new_layout)
    }

    #[cfg(target_os = "macos")]
    unsafe fn shrink_in_place(
        &mut self,
        ptr: *mut u8,
        layout: Layout,
        new_layout: Layout,
    ) -> Result<(), CannotReallocInPlace> {
        debug_assert!(
            layout.size() > 0,
            "shrink_in_place: size of layout must be non-zero"
        );
        debug_assert!(
            new_layout.size() > 0,
            "shrink_in_place: size of new_layout must be non-zero"
        );
        debug_assert!(new_layout.size() <= layout.size());
        debug_assert_eq!(new_layout.align(), layout.align());

        let old_size = next_multiple(layout.size(), self.pagesize);
        let new_size = next_multiple(new_layout.size(), self.pagesize);
        if new_size < old_size {
            let diff = old_size - new_size;
            let ptr = (ptr as usize + new_size) as *mut u8;
            unmap(ptr, diff);
        }
        Ok(())
    }
}

unsafe impl<'a> UntypedObjectAlloc for &'a MapAlloc {
    fn layout(&self) -> Layout {
        self.obj_layout.clone()
    }

    unsafe fn alloc(&mut self) -> Result<NonNull<u8>, Exhausted> {
        // TODO: There's probably a method that does this more cleanly.
        match self.alloc_excess(self.layout()) {
            Ok(Excess(ptr, _)) => Ok(NonNull::new_unchecked(ptr)),
            Err(AllocErr::Exhausted { .. }) => Err(Exhausted),
            Err(AllocErr::Unsupported { .. }) => unreachable!(),
        }
    }

    unsafe fn dealloc(&mut self, ptr: NonNull<u8>) {
        <&MapAlloc as Alloc>::dealloc(self, ptr.as_ptr(), self.obj_layout.clone());
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

    unsafe fn realloc(
        &mut self,
        ptr: *mut u8,
        layout: Layout,
        new_layout: Layout,
    ) -> Result<*mut u8, AllocErr> {
        <&MapAlloc as Alloc>::realloc(&mut (&*self), ptr, layout, new_layout)
    }

    unsafe fn grow_in_place(
        &mut self,
        ptr: *mut u8,
        layout: Layout,
        new_layout: Layout,
    ) -> Result<(), CannotReallocInPlace> {
        <&MapAlloc as Alloc>::grow_in_place(&mut (&*self), ptr, layout, new_layout)
    }

    unsafe fn shrink_in_place(
        &mut self,
        ptr: *mut u8,
        layout: Layout,
        new_layout: Layout,
    ) -> Result<(), CannotReallocInPlace> {
        <&MapAlloc as Alloc>::shrink_in_place(&mut (&*self), ptr, layout, new_layout)
    }
}

unsafe impl UntypedObjectAlloc for MapAlloc {
    fn layout(&self) -> Layout {
        <&MapAlloc as UntypedObjectAlloc>::layout(&(&*self))
    }

    unsafe fn alloc(&mut self) -> Result<NonNull<u8>, Exhausted> {
        <&MapAlloc as UntypedObjectAlloc>::alloc(&mut (&*self))
    }

    unsafe fn dealloc(&mut self, ptr: NonNull<u8>) {
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

// NOTE on mapping at the NULL address: A previous version of this code explicitly checked for NULL
// being returned from mmap (on both Linux and Mac). However, it was discovered that the POSIX
// standard and the Linux manpage both guarantee that NULL will never be returned so long as the
// MAP_FIXED flag is not passed. If this ever becomes a problem in the future as we support new
// platforms, it may be helpful to see how this was dealt with in the past. The last version of the
// code that explicitly handled this condition was at commit 2caa95624b3d, and the logic was in the
// alloc_helper method. A similar check was performed for Linux's mremap call in the realloc_helper
// method.

#[cfg(target_os = "linux")]
unsafe fn map(size: usize, perms: i32, commit: bool) -> Option<*mut u8> {
    use libc::{ENOMEM, MAP_ANONYMOUS, MAP_FAILED, MAP_POPULATE, MAP_PRIVATE};

    // TODO: Figure out when it's safe to pass MAP_UNINITIALIZED (it's not defined in all
    // versions of libc). Be careful about not invalidating alloc_zeroed.

    let flags = if commit { MAP_POPULATE } else { 0 };

    let ptr = libc::mmap(
        ptr::null_mut(),
        size,
        perms,
        MAP_ANONYMOUS | MAP_PRIVATE | flags,
        -1,
        0,
    );

    if ptr == MAP_FAILED {
        if errno().0 == ENOMEM {
            None
        } else {
            panic!("mmap failed: {}", errno())
        }
    } else {
        // On Linux, if the MAP_FIXED flag is not supplied, mmap will never return NULL. From the
        // Linux manpage: "The portable way to create a mapping is to specify addr as 0 (NULL), and
        // omit MAP_FIXED from flags. In this case, the system chooses the address for the mapping;
        // the address is chosen so as not to conflict with any existing mapping, and will not be
        // 0."
        assert_ne!(ptr, ptr::null_mut(), "mmap returned NULL");
        Some(ptr as *mut u8)
    }
}

#[cfg(target_os = "macos")]
unsafe fn map(size: usize, perms: i32, do_commit: bool) -> Option<*mut u8> {
    use libc::{ENOMEM, MAP_ANON, MAP_FAILED, MAP_PRIVATE};

    let ptr = libc::mmap(ptr::null_mut(), size, perms, MAP_ANON | MAP_PRIVATE, -1, 0);

    if ptr == MAP_FAILED {
        if errno().0 == ENOMEM {
            None
        } else {
            panic!("mmap failed: {}", errno())
        }
    } else {
        // POSIX-compliant mmap implementations cannot return NULL if the MAP_FIXED flag is not
        // supplied. From the POSIX standard
        // (http://pubs.opengroup.org/onlinepubs/009695399/functions/mmap.html): "When the
        // implementation selects a value for pa, it never places a mapping at address 0, nor does
        // it replace any extant mapping."
        assert_ne!(ptr, ptr::null_mut(), "mmap returned NULL");
        if do_commit {
            // Mac mmap doesn't have an equivalent of the Linux MAP_POPULATE flag,
            // so we commit memory in a separate step.
            commit(ptr as *mut u8, size);
        }
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
unsafe fn map(size: usize, perms: u32, commit: bool) -> Option<*mut u8> {
    use kernel32::VirtualAlloc;
    use winapi::winnt::{MEM_COMMIT, MEM_RESERVE};

    let typ = MEM_RESERVE | if commit { MEM_COMMIT } else { 0 };

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
    if ptr.is_null() {
        None
    } else {
        Some(ptr)
    }
}

#[cfg(target_os = "linux")]
unsafe fn remap(ptr: *mut u8, old_size: usize, new_size: usize, in_place: bool) -> Option<*mut u8> {
    let flags = if !in_place { libc::MREMAP_MAYMOVE } else { 0 };
    let result = libc::mremap(ptr as *mut _, old_size, new_size, flags);
    if result == libc::MAP_FAILED {
        let err = errno();
        if err.0 == libc::ENOMEM {
            None
        } else {
            panic!("mremap failed: {}", err)
        }
    } else {
        // The Linux manpage implies that mremap cannot return NULL without the MREMAP_FIXED flag,
        // although it's not 100% clear. "[MREMAP_FIXED] serves a similar purpose to the MAP_FIXED
        // flag of mmap(2)." Since a lack of MAP_FIXED in mmap requires mmap to not return NULL,
        // we interpret this line from the mremap manpage to imply that a similar requirement holds
        // for mremap. In any case, this assertion will catch us if it turns out we're wrong.
        assert_ne!(ptr, ptr::null_mut(), "mremap returned NULL");
        Some(result as *mut u8)
    }
}

#[cfg(any(target_os = "linux", target_os = "macos"))]
unsafe fn unmap(ptr: *mut u8, size: usize) {
    // NOTE: Don't inline the call to munmap; then errno might be called before munmap.
    let ret = libc::munmap(ptr as *mut _, size);
    assert_eq!(ret, 0, "munmap failed: {}", errno());
}

#[cfg(windows)]
unsafe fn unmap(ptr: *mut u8, _size: usize) {
    use kernel32::{GetLastError, VirtualFree};
    use winapi::winnt::MEM_RELEASE;

    // NOTE: VirtualFree, when unmapping memory (as opposed to decommitting it), can only operate
    // on an entire region previously mapped with VirtualAlloc. As a result, 'ptr' must have been
    // previously returned by VirtualAlloc, and no length is needed since it is known by the kernel
    // (VirtualFree /requires/ that if the third argument is MEM_RELEASE, the second is 0).
    let ret = VirtualFree(ptr as *mut _, 0, MEM_RELEASE);
    assert_ne!(
        ret,
        0,
        "Call to VirtualFree({:?}, 0, MEM_RELEASE) failed with error code {}.",
        ptr,
        GetLastError()
    );
}

#[cfg_attr(target_os = "linux", allow(unused))]
#[cfg(any(target_os = "linux", target_os = "macos"))]
unsafe fn protect(ptr: *mut u8, size: usize, perm: perms::Perm) {
    // NOTE: Don't inline the call to mprotect; then errno might be called before mprotect.
    let ret = libc::mprotect(ptr as *mut _, size, perm);
    assert_eq!(ret, 0, "mprotect failed: {}", errno());
}

#[cfg(windows)]
unsafe fn protect(ptr: *mut u8, size: usize, perm: perms::Perm) {
    use kernel32::{GetLastError, VirtualProtect};

    let mut _old_perm: winapi::DWORD = 0;
    #[cfg(target_pointer_width = "64")]
    type U = u64;
    #[cfg(target_pointer_width = "32")]
    type U = u32;
    let ret = VirtualProtect(ptr as *mut _, size as U, perm, &mut _old_perm as *mut _);
    assert_ne!(
        ret,
        0,
        "Call to VirtualProtect({:?}, {}, {}, {}) failed with error code {}.",
        ptr,
        size,
        perm,
        _old_perm,
        GetLastError()
    );
}

#[cfg(any(target_os = "linux", target_os = "macos"))]
unsafe fn commit(ptr: *mut u8, size: usize) {
    use libc::{madvise, MADV_WILLNEED};
    madvise(ptr as *mut _, size, MADV_WILLNEED);
}

#[cfg(windows)]
unsafe fn commit(ptr: *mut u8, size: usize, perms: u32) {
    use kernel32::VirtualAlloc;
    use winapi::winnt::MEM_COMMIT;

    let ret = VirtualAlloc(ptr as *mut _, size as WindowsSize, MEM_COMMIT, perms);
    assert_eq!(ret as *mut u8, ptr);
}

#[cfg(target_os = "linux")]
unsafe fn uncommit(ptr: *mut u8, size: usize) {
    use libc::{madvise, MADV_DONTNEED};

    // TODO: Other options such as MADV_FREE are available on newer versions of Linux. Is there
    // a way that we can use those when available? Is that even desirable?
    madvise(ptr as *mut _, size, MADV_DONTNEED);
}

#[cfg(target_os = "macos")]
unsafe fn uncommit(ptr: *mut u8, size: usize) {
    use libc::{madvise, MADV_FREE};
    madvise(ptr as *mut _, size, MADV_FREE);
}

#[cfg(windows)]
unsafe fn uncommit(ptr: *mut u8, size: usize) {
    use kernel32::{GetLastError, VirtualFree};
    use winapi::winnt::MEM_DECOMMIT;

    let ret = VirtualFree(ptr as *mut _, size as WindowsSize, MEM_DECOMMIT);
    assert_ne!(
        ret,
        0,
        "Call to VirtualFree({:?}, {}, MEM_DECOMMIT) failed with error code {}.",
        ptr,
        size,
        GetLastError()
    );
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
