// Copyright 2017-2018 the authors. See the 'Copyright and license' section of the
// README.md file at the top-level directory of this repository.
//
// Licensed under the Apache License, Version 2.0 (the LICENSE-APACHE file) or
// the MIT license (the LICENSE-MIT file) at your option. This file may not be
// copied, modified, or distributed except according to those terms.

use sysconf::page::pagesize;
use super::*;
use super::perms::*;

extern crate test;

#[cfg_attr(windows, allow(unused))]
use std::time::{Duration, Instant};
#[cfg_attr(windows, allow(unused))]
use self::test::Bencher;

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
        let got = *ptr.offset(i as isize);
        let want = (i & 0xff) as u8;
        assert_eq!(
            got, want,
            "mismatch at byte {} in block {:?}: got {}, want {}",
            i, ptr, got, want
        );
    }
}

// Test that the given range is readable and writable, and that writes can be read back.
unsafe fn test_write_read(ptr: *mut u8, size: usize) {
    test_write(ptr, size);
    test_read(ptr, size);
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
        let mut ptr = map(pagesize(), PROT_READ_WRITE, false).unwrap();
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
        ptr = map(16 * pagesize(), PROT_READ_WRITE, false).unwrap();
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
        let mut ptr = map(5 * pagesize(), PROT_READ_WRITE, false).unwrap();
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
        let t0 = Instant::now();
        ptr = map(size, PROT_READ_WRITE, false).unwrap();
        // In tests on a 2016 MacBook Pro (see bench_large_map), a 2^31 byte map/unmap pair
        // took ~5 usec natively (Mac OS X) and ~350 ns in a Linux VM. Thus, 1 ms is a safe
        // upper bound.
        let diff = Instant::now().duration_since(t0);
        let target = Duration::from_millis(1);
        assert!(diff < target, "duration: {:?}", diff);
        test_valid_map_address(ptr);
        test_zero_filled(ptr.offset((size / 2) as isize), pagesize());
        unmap(ptr, size);
    }
}

#[test]
fn test_realloc_small() {
    unsafe fn test(read: bool, write: bool, exec: bool) {
        let builder = MapAllocBuilder::default()
            .read(read)
            .write(write)
            .exec(exec);
        #[cfg(windows)]
        let builder = builder.commit(true);
        let mut alloc = builder.build();
        #[cfg(any(target_os = "linux", windows))]
        let perm = get_perm(read, write, exec);

        let test_contents = |ptr: *mut u8, size: usize| {
            if read && write {
                test_write_read(ptr, size);
            } else if read {
                test_zero_filled(ptr, size);
            } else if write {
                test_write(ptr, size);
            }
        };

        let small = Layout::array::<u8>(1).unwrap();
        let medium = Layout::array::<u8>(2048).unwrap();
        let large = Layout::array::<u8>(4096).unwrap();
        let ptr = <MapAlloc as Alloc>::alloc(&mut alloc, medium.clone()).unwrap();
        test_valid_map_address(ptr);
        if read {
            test_zero_filled(ptr, medium.size());
        }
        test_contents(ptr, medium.size());
        #[cfg(any(target_os = "linux", windows))]
        assert_block_perm(ptr, medium.size(), perm);
        alloc
            .shrink_in_place(ptr, medium.clone(), small.clone())
            .unwrap();
        if read && write {
            // First check to make sure the old contents are still there. This requires both
            // read (in order to read the contents) and write (or else we wouldn't have written
            // the special pattern).
            test_read(ptr, small.size());
        }
        test_contents(ptr, small.size());
        #[cfg(any(target_os = "linux", windows))]
        assert_block_perm(ptr, small.size(), perm);
        alloc
            .grow_in_place(ptr, small.clone(), large.clone())
            .unwrap();
        if read && write {
            // First check to make sure the old contents are still there. This requires both
            // read (in order to read the contents) and write (or else we wouldn't have written
            // the special pattern). We can only check the smaller size because all pages
            // beyond that may have been altered.
            test_read(ptr, small.size());
        }
        test_contents(ptr, large.size());
        #[cfg(any(target_os = "linux", windows))]
        assert_block_perm(ptr, large.size(), perm);
        let old_ptr = ptr;
        let ptr = alloc.realloc(ptr, large.clone(), small.clone()).unwrap();
        if cfg!(target_os = "linux") {
            assert_eq!(old_ptr, ptr);
        }
        if read && write {
            // First check to make sure the old contents are still there. This requires both
            // read (in order to read the contents) and write (or else we wouldn't have written
            // the special pattern).
            test_read(ptr, small.size());
        }
        test_contents(ptr, small.size());
        #[cfg(any(target_os = "linux", windows))]
        assert_block_perm(ptr, small.size(), perm);
        <MapAlloc as Alloc>::dealloc(&mut alloc, ptr, small.clone());
    }

    unsafe {
        test(false, false, false);
        test(true, false, false);
        test(false, true, false);
        test(false, false, true);
        test(true, true, false);
        test(true, false, true);
        test(false, true, true);
        test(true, true, true);
    }
}

// Only run this test on its own (not concurrently with other tests) because when testing
// grow_in_place, it assumes that certain unmapped pages will not be mapped in between calling
// shrink_in_place and grow_in_place. When other tests are running concurrently, this can be
// violated. This can be acheived by setting the environment variable RUST_TEST_THREADS=1.
#[test]
fn test_realloc_large() {
    unsafe fn test(read: bool, write: bool, exec: bool) {
        let builder = MapAllocBuilder::default()
            .read(read)
            .write(write)
            .exec(exec);
        #[cfg(windows)]
        let builder = builder.commit(true);
        let mut alloc = builder.build();
        #[cfg(any(target_os = "linux", windows))]
        let perm = get_perm(read, write, exec);

        let test_contents = |ptr: *mut u8, size: usize| {
            if read && write {
                test_write_read(ptr, size);
            } else if read {
                test_zero_filled(ptr, size);
            } else if write {
                test_write(ptr, size);
            }
        };

        let small = Layout::array::<u8>(alloc.pagesize).unwrap();
        let medium = Layout::array::<u8>(alloc.pagesize * 8).unwrap();
        let large = Layout::array::<u8>(alloc.pagesize * 16).unwrap();
        let mut ptr = <MapAlloc as Alloc>::alloc(&mut alloc, large.clone()).unwrap();
        test_valid_map_address(ptr);
        if read {
            test_zero_filled(ptr, large.size());
        }
        test_contents(ptr, large.size());
        #[cfg(any(target_os = "linux", windows))]
        assert_block_perm(ptr, large.size(), perm);
        if cfg!(target_os = "linux") {
            alloc
                .shrink_in_place(ptr, large.clone(), small.clone())
                .unwrap();
            ptr = alloc.realloc(ptr, large.clone(), small.clone()).unwrap();
            if read && write {
                // First check to make sure the old contents are still there. This requires both
                // read (in order to read the contents) and write (or else we wouldn't have written
                // the special pattern).
                test_read(ptr, small.size());
            }
            test_contents(ptr, small.size());
            #[cfg(any(target_os = "linux", windows))]
            assert_block_perm(ptr, small.size(), perm);
            alloc
                .grow_in_place(ptr, small.clone(), medium.clone())
                .unwrap();
            if read && write {
                // First check to make sure the old contents are still there. This requires both
                // read (in order to read the contents) and write (or else we wouldn't have written
                // the special pattern). We can only check the smaller size because all pages
                // beyond that may have been altered.
                test_read(ptr, small.size());
            }
            test_contents(ptr, medium.size());
            #[cfg(any(target_os = "linux", windows))]
            assert_block_perm(ptr, medium.size(), perm);
        } else {
            ptr = alloc.realloc(ptr, large.clone(), medium.clone()).unwrap();
            test_valid_map_address(ptr);
            if read && write {
                // First check to make sure the old contents are still there. This requires both
                // read (in order to read the contents) and write (or else we wouldn't have written
                // the special pattern). We can only check the smaller size because all pages
                // beyond that may have been altered.
                test_read(ptr, medium.size());
            }
        }
        let old_ptr = ptr;
        let ptr = alloc.realloc(ptr, medium.clone(), small.clone()).unwrap();
        if cfg!(target_os = "linux") {
            assert_eq!(old_ptr, ptr);
        }
        if read && write {
            // First check to make sure the old contents are still there. This requires both read
            // (in order to read the contents) and write (or else we wouldn't have written the
            // special pattern).
            test_read(ptr, small.size());
        }
        test_contents(ptr, small.size());
        #[cfg(any(target_os = "linux", windows))]
        assert_block_perm(ptr, small.size(), perm);
        let ptr = alloc.realloc(ptr, small.clone(), large.clone()).unwrap();
        if cfg!(target_os = "linux") {
            assert_eq!(old_ptr, ptr);
        }
        if read && write {
            // First check to make sure the old contents are still there. This requires both read
            // (in order to read the contents) and write (or else we wouldn't have written the
            // special pattern). We can only check the smaller size because all pages beyond that
            // may have been altered.
            test_read(ptr, small.size());
        }
        test_contents(ptr, large.size());
        #[cfg(any(target_os = "linux", windows))]
        assert_block_perm(ptr, large.size(), perm);

        if cfg!(target_os = "linux") || cfg!(target_os = "macos") {
            // This test specifically tests the Linux implementation's use of mremap. It also
            // happens to work on Mac, so we run it on Mac as well. It doesn't work on Windows
            // because it relies on the ability to treat mappings as subdividable, which they are
            // not on Windows (e.g., you can't unmap half of an existing mapping).

            // Treating the first page as its own allocation, grow that allocation to multiple
            // pages. Since we know there's a second mapped page right after it, remap will be
            // unable to simply grow the mapping in place, and will be forced to move it. This way,
            // we can test that when a mapping is moved, its contents are not modified.
            let remaining = ptr.offset(alloc.pagesize as isize);
            let remaining_layout = Layout::array::<u8>(large.size() - alloc.pagesize).unwrap();
            let new = alloc.realloc(ptr, small.clone(), medium.clone()).unwrap();
            if read && write {
                // First check to make sure the old contents are still there. This requires both
                // read (in order to read the contents) and write (or else we wouldn't have written
                // the special pattern).
                test_read(new, small.size());
                // Test to make sure the rest of the old allocation wasn't modified.
                test_read(remaining, remaining_layout.size());
            }
            #[cfg(any(target_os = "linux", windows))]
            assert_block_perm(new, medium.size(), perm);
            <MapAlloc as Alloc>::dealloc(&mut alloc, new, medium.clone());
            <MapAlloc as Alloc>::dealloc(&mut alloc, remaining, remaining_layout.clone());
        }
    }

    unsafe {
        test(false, false, false);
        test(true, false, false);
        test(false, true, false);
        test(false, false, true);
        test(true, true, false);
        test(true, false, true);
        test(false, true, true);
        test(true, true, true);
    }
}

#[cfg(feature = "large-align")]
#[test]
fn test_large_align() {
    let mut alloc = MapAllocBuilder::default().commit(true).build();
    // test with a large alignment so that the probability of getting this
    // alignment by chance is low.
    const MB64: usize = 1 << 25;
    let layout = Layout::from_size_align(MB64, MB64).unwrap();
    unsafe {
        let ptr = <MapAlloc as Alloc>::alloc(&mut alloc, layout.clone()).unwrap();
        assert_eq!(ptr as usize % MB64, 0, "ptr: {:?}", ptr);
        test_zero_filled(ptr, MB64);
        test_write_read(ptr, MB64);
        <MapAlloc as Alloc>::dealloc(&mut alloc, ptr, layout.clone());
    }
}

#[test]
fn test_commit() {
    unsafe {
        // Check that:
        // - Mapping and committing a single page works
        // - The returned pointer is non-null
        // - The returned pointer is page-aligned
        // - We can read that page, and it is zero-filled (on Unix, this test is trivial, but
        //   on Windows, it ensures that map properly committed the page)
        let mut ptr = map(pagesize(), PROT_READ_WRITE, true).unwrap();
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
        ptr = map(pagesize(), PROT_READ_WRITE, false).unwrap();
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
        // Check that:
        // - Mapping a single read-only page works
        // - The returned pointer is non-null
        // - The returned pointer is page-aligned
        // - We can read the page, and it is zero-filled (on Windows, after the page is committed)
        // - It has the proper permissions (on Linux)
        let mut ptr = map(pagesize(), PROT_READ, false).unwrap();
        test_valid_map_address(ptr);
        #[cfg(windows)]
        commit(ptr, pagesize(), PROT_READ);
        test_zero_filled(ptr, pagesize());
        #[cfg(any(target_os = "linux", windows))]
        assert_block_perm(ptr, pagesize(), PROT_READ);
        unmap(ptr, pagesize());

        // Check that:
        // - Mapping a single write-only page works
        // - The returned pointer is non-null
        // - The returned pointer is page-aligned
        // - We can write to the page (on Windows, after the page is committed)
        // - It has the proper permissions (on Linux)
        ptr = map(pagesize(), PROT_WRITE, false).unwrap();
        test_valid_map_address(ptr);
        #[cfg(windows)]
        commit(ptr, pagesize(), PROT_WRITE);
        test_write(ptr, pagesize());
        #[cfg(any(target_os = "linux", windows))]
        assert_block_perm(ptr, pagesize(), PROT_WRITE);
        unmap(ptr, pagesize());

        // Check that:
        // - Mapping a single read-write page works
        // - The returned pointer is non-null
        // - The returned pointer is page-aligned
        // - We can read the page, and it is zero-filled (on Windows, after the page is committed)
        // - We can write to the page, and those writes are properly read back
        // - It has the proper permissions (on Linux)
        ptr = map(pagesize(), PROT_READ_WRITE, false).unwrap();
        test_valid_map_address(ptr);
        #[cfg(windows)]
        commit(ptr, pagesize(), PROT_READ_WRITE);
        test_zero_filled(ptr, pagesize());
        test_write_read(ptr, pagesize());
        #[cfg(any(target_os = "linux", windows))]
        assert_block_perm(ptr, pagesize(), PROT_READ_WRITE);
        unmap(ptr, pagesize());

        #[cfg(target_os = "linux")]
        {
            fn test_perms(perm: Perm) {
                unsafe {
                    // Perform the same checks as above on arbitrary permissions.
                    let ptr = map(pagesize(), perm, false).unwrap();
                    test_valid_map_address(ptr);
                    assert_block_perm(ptr, pagesize(), perm);
                    unmap(ptr, pagesize());
                }
            }

            // test all the permissions we haven't tested already
            test_perms(PROT_NONE);
            test_perms(PROT_EXEC);
            test_perms(PROT_READ_EXEC);
            test_perms(PROT_WRITE_EXEC);
            test_perms(PROT_READ_WRITE_EXEC);
        }
    }
}

#[cfg(not(windows))]
#[test]
#[should_panic]
fn test_map_panic_zero() {
    unsafe {
        // Check that zero length causes map to panic. On Windows, our map implementation never
        // panics.
        map(0, PROT_READ_WRITE, false);
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
        map(MAX, PROT_READ_WRITE, false);
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
        let ptr = map(pagesize(), PROT_READ_WRITE, false).unwrap();
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
#[bench]
#[ignore]
fn bench_large_map(b: &mut Bencher) {
    // Determine the speed of mapping a large region of memory so that we can tune the timeout
    // in test_map_non_windows.
    b.iter(|| unsafe {
        let ptr = map(1 << 29, PROT_READ_WRITE, false).unwrap();
        unmap(ptr, 1 << 29);
    })
}

// assert_block_perm asserts that every page in the block starting at ptr with the given size has
// the given set of permissions.
#[cfg(target_os = "linux")]
fn assert_block_perm(ptr: *mut u8, size: usize, perm: Perm) {
    let ptr = ptr as usize;
    for block in perms() {
        // If this Block overlaps with the block being queried, then since a Block can only have
        // a single set of permissions, then their permissions must match.
        let disjoint = (ptr + size) <= block.begin as usize || block.end as usize <= ptr;
        if !disjoint {
            assert_eq!(perm, block.perm);
        }
    }
}

#[cfg(windows)]
fn assert_block_perm(ptr: *mut u8, size: usize, perm: Perm) {
    unsafe {
        use std::mem;
        let mut meminfo: winapi::winnt::MEMORY_BASIC_INFORMATION = mem::uninitialized();
        let mbi_size = mem::size_of::<winapi::winnt::MEMORY_BASIC_INFORMATION>();
        let ret = kernel32::VirtualQuery(ptr as *mut _, &mut meminfo as *mut _, mbi_size as u64);
        assert_ne!(ret, 0);

        assert!(meminfo.RegionSize >= size as u64);
        assert_eq!(meminfo.Protect, perm);
    }
}

// A Block represents a mapped block of memory with a particular set of permissions. The range is
// inclusive/exclusive (i.e., [begin, end)).
#[cfg(target_os = "linux")]
#[derive(Debug)]
struct Block {
    begin: *mut u8,
    end: *mut u8,
    perm: Perm,
}

#[cfg(target_os = "linux")]
#[test]
fn test_perms_fn() {
    perms();
}

// perms gets a list of the mapped blocks for this process and their associated permissions.
#[cfg(target_os = "linux")]
fn perms() -> Vec<Block> {
    // perms works by parsing /proc/<pid>/maps, which has lines like
    // 7f818a324000-7f818a326000 rw-p 001c4000 08:01 24360                      /lib/x86_64-linux-gnu/libc-2.23.so
    // We care about the first three fields: the starting address, the ending address, and the
    // permissions set.

    use std::collections::HashMap;

    let mut map = HashMap::new();
    map.insert(String::from("---p"), PROT_NONE);
    map.insert(String::from("r--p"), PROT_READ);
    map.insert(String::from("-w-p"), PROT_WRITE);
    map.insert(String::from("--xp"), PROT_EXEC);
    map.insert(String::from("rw-p"), PROT_READ_WRITE);
    map.insert(String::from("r-xp"), PROT_READ_EXEC);
    map.insert(String::from("-wxp"), PROT_WRITE_EXEC);
    map.insert(String::from("rwxp"), PROT_READ_WRITE_EXEC);

    let mut blocks = Vec::new();

    use std::fs::File;
    use std::io::Read;
    let mut output = String::new();
    let mut file = File::open(format!("/proc/{}/maps", unsafe { libc::getpid() })).unwrap();
    file.read_to_string(&mut output).unwrap();

    for line in output.lines() {
        let first_dash = line.find('-').unwrap();
        let (first, mut remaining) = line.split_at(first_dash);
        remaining = remaining.split_at(1).1; // skip over the dash
        let first_space = remaining.find(' ').unwrap();
        let (second, mut third) = remaining.split_at(first_space);
        // skip over the space and truncate the rest of the line
        third = third.split_at(1).1.split(' ').next().unwrap();

        let begin = usize::from_str_radix(first, 16).unwrap() as *mut u8;
        let end = usize::from_str_radix(second, 16).unwrap() as *mut u8;
        let perm = *map.get(third).unwrap();
        blocks.push(Block { begin, end, perm });
    }

    blocks
}
