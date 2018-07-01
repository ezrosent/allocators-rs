// Copyright 2017-2018 the authors. See the 'Copyright and license' section of the
// README.md file at the top-level directory of this repository.
//
// Licensed under the Apache License, Version 2.0 (the LICENSE-APACHE file) or
// the MIT license (the LICENSE-MIT file) at your option. This file may not be
// copied, modified, or distributed except according to those terms.

//! A testing framework to detect memory corruption issues in object allocators.
//!
//! See `TestBuilder` for documentation on running tests.

extern crate libc;
extern crate object_alloc;
extern crate rand;
extern crate test;
extern crate twox_hash;

use self::object_alloc::ObjectAlloc;
use self::test::black_box;
use std::fmt;
use std::fmt::{Debug, Formatter};
use std::ptr::NonNull;

/// A type to test for memory corruption issues.
///
/// The `CorruptionTester` is designed to be able to detect when an allocated value has been
/// corrupted, which includes being moved in memory, dropped multiple times, or simply having its
/// memory overwritten with other data.
///
/// The `CorruptionTester` is wrapped in either a `CorruptionTesterDefault` or a
/// `CorruptionTesterUnsafe`. The former initializes using the `Default` trait, and the latter
/// using a custom `unsafe_default` initializer.
///
/// # Lifecycle
///
/// In order to perform a corruption test, an `ObjectAlloc` is constructed to allocate
/// `CorruptionTester` values. The `Tester` object and its methods perform the work of allocating
/// and deallocating while checking for various corruption errors.
///
/// Each `CorruptionTester` object is designed to have a very particular lifecycle:
///
/// 1. First, the object is created using its wrapper's initializer (`CorruptionTesterDefault`'s
///    `default` method or the `unsafe_default` function). The resulting instance is in the `New`
///    state.
///
///    In the case of `default`, since the object is returned by value, it does not encode any
///    information about its location in memory since, during the call to `default`, it's
///    impossible to know where it will be placed. While it is in this state, its memory can be
///    copied without detection. This is a fundamental limitation of this design. A more aggressive
///    design might have `default` register each created object in a global data structure, but
///    this would seriously hurt the performance of this test.
///
///    In the case of `unsafe_default`, the object's address is known at initialization time. An
///    object initialized using `unsafe_default` encodes information about its location in memory,
///    and is thus able to detect whether a valid object has been copied from its original
///    location.
/// 2. After being created in the `New` state, an object may be returned from a call to `alloc`.
///    At this point, the caller will verify that it is in the `New` state, and will transition it
///    to the `Valid` state.
///
///    Since, at this point, the caller knows the location in memory where the object is stored,
///    while transitioning to the `Valid` state, an object which was initialized with `default` is
///    is updated to reflect its address. From this point forward, if the object's underlying
///    memory is moved or copied elsewhere, it will be detected.
/// 3. After being moved to the `Valid` state, the object will be freed back to the allocator. The
///    allocator may drop the object, but it also may cache the object for use in future `alloc`
///    calls. Thus, the test accepts objects returned from `alloc` in either the `New` or `Valid`
///    states.
/// 4. Eventually, every object will be dropped. `CorruptionTester` implements a custom drop method
///    that transitions it to the `Dropped` state. The drop method expects to find the object in
///    either the `New` state (indicating that it was constructed but never returned from an
///    `alloc` call) or the `Valid` state (indicating that it was returned from an `alloc` call).
/// 5. When an object is initialized using `unsafe_default`, it is verified that the memory being
///    used for this object is either not a valid `CorruptionTester` object, or otherwise is a
///    valid object in the `Dropped` state. This ensures that a new object does not clobber an
///    existing object before that object is dropped.
///
/// At many points throughout this lifecycle, the validity of the object is verified. This allows,
/// for example, for a `CorruptionTester`'s drop method to detect that it is being called on an
/// unconstructed or corrupted region of memory. Many other error conditions can also be detected.
///
/// # In-Memory Representation
///
/// While the `CorruptionTester` takes a type parameter, this parameter is only actually used to
/// allow the `CorruptionTester` to have different sizes and alignments with which to test an
/// allocator. In actuality, a custom memory layout is used that shares nothing with `T` other than
/// its size.
///
/// Each `CorruptionTester` object comprises an 8-byte header (the `Header` type) and an array of 1
/// or more bytes. The header contains two fields: the `state_nonce` and the `hash`, while the
/// array of bytes is filled with random data.
///
/// The `state_nonce` is a 32-bit value used to keep track of the current state of the object
/// (`New`, `Valid`, or `Dropped`). Each state has its own fixed random nonce (see `NONCE_XXX`).
/// These values are random to reduce the likelihood that they will appear in memory by accident.
///
/// The `hash` is used to ensure that the entire object remains uncorrupted. In the `New` state, it
/// is initialized depending on what initialization method is used. If `default` is used, then as
/// discussed before, the final address of the object is not known, and thus `hash` is initialized
/// to the hash of the `state_nonce` and the random array. If `unsafe_default` is used, since the
/// final address is known, `hash` is initialized to the hash of the object's address, the
/// `state_nonce`, and the random array.
///
/// In the `Valid` and `Dropped` states, it is similarly set to the hash of the object's address,
/// the `state_nonce`, and the random array. This allows a number of different types of memory
/// corruption to be detected (note that when we say "will be detected" here, we really mean "very
/// likely to be detected" since there's always a small probability that a random sequence of bytes
/// will constitute a valid object):
///
/// * In the `New` state when initialized with `default`, it is only guaranteed that a given object
///   is a valid object. There is no guarantee that another valid object has not been copied over
///   it. However, this still ensures that any modifications to the object *other* than the
///   wholesale copying of another valid object will be detected.
/// * In the `Valid` and `Dropped` states, and in the `New` state when initialized with
///   `unsafe_default`, since the `hash` is dependent on the address of the object, another object
///   in the same state - but residing in a different memory location - will have a different
///   `hash`. Thus, copying a valid object over another will be detected.
///
/// # Corruption Test Algorithm
///
/// The corruption test itself - as implemented in `TestBuilder`'s `test` method - works by
/// randomly allocating and deallocating many objects and constantly checking their validity. For
/// each of `N` iterations, it randomly decides to either allocate a new object or deallocate an
/// existing object (if any exist). After the `N` iterations are up, it deallocates any remaining
/// objects.
///
/// After all objects have been deallocated, the test drops the allocator itself. This should cause
/// all existing, constructed objects to be dropped. In order to verify this, the test walks
/// through all objects that were ever allocated to ensure that they were properly dropped. It does
/// this by checking to see that the memory is in one of three states: unmapped (indicating that
/// the allocator already freed the underlying memory pages back to the kernel), `Invalid`
/// (indicating that the memory for the object has been modified after being dropped, which is
/// valid), or `Dropped`.
///
/// Note that this check is unfortunately incomplete. An object could be overwritten without being
/// dropped, placing it in the `Invalid` state. Its backing memory pages could also be freed back
/// to the kernel without being dropped. Neither of these will be detected.
union CorruptionTester<T: Copy> {
    // ensure that a CorruptionTester is always large enough even if T isn't
    _min: [u8; MIN_SIZE],
    _t: T,
}

/// An object for use in corruption tests that implements `Default`.
///
/// `CorruptionTesterDefault` objects are objects that are allocated by an `ObjectAlloc` in order
/// to test that `ObjectAlloc` for corruption issues. `CorruptionTesterDefault` implements
/// `Default`, so new instances can be constructed using the `default` method.
pub struct CorruptionTesterDefault<T: Copy = [u8; MIN_SIZE]>(CorruptionTester<T>);

/// An object for use in corruption tests constructed by `unsafe_default`.
///
/// `CorruptionTesterUnsafe` objects are objects that are allocated by an `ObjectAlloc` in order
/// to test that `ObjectAlloc` for corruption issues. New `CorruptionTesterUnsafe` instances can
/// be constructed using the `unsafe_default` function.
pub struct CorruptionTesterUnsafe<T: Copy = [u8; MIN_SIZE]>(CorruptionTester<T>);

// This annoying setup is to sidestep the "private trait in a public interface" issue (with respect
// to CorruptionTesterWrapper's use in the quickcheck module).
use self::wrapper::*;
mod wrapper {
    // Defined here (instead of in the parent) so that it's not a warning (and, in the future, an
    // error) to have CorruptionTesterWrapper's state method return a State object (if State were
    // defined in the parent module, it'd be private, which could make its use in
    // CorruptionTesterWrapper, which is public, a "private type in a public interface" issue).
    #[derive(PartialEq, Debug)]
    pub enum State {
        New,
        Valid,
        Dropped,
        Invalid,
    }

    /// A trait that abstracts over `CorruptionTesterDefault` and `CorruptionTesterUnsafe`.
    pub trait CorruptionTesterWrapper {
        /// Returns the object's state and `random_nonce`.
        fn state(&self) -> State;

        // TODO: It looks like we only call with check_state = true in the 'tests' module. Consider
        // removing the check_state parameter.

        /// Updates the object's state from `New` to `Valid`.
        ///
        /// If `check_state` is true, `update_new` also verifies that the object's current state is
        /// `New`. The `random_nonce` is returned.
        fn update_new(&mut self, check_state: bool);
    }
}

// MIN_SIZE is large enough to hold a Header and a one-byte random nonce.
const MIN_SIZE: usize = ::std::mem::size_of::<Header>() + 1;

#[derive(Debug)]
struct Header {
    state_nonce: u32,
    hash: u32,
}

#[derive(PartialEq)]
enum Initializer {
    Default,
    Unsafe,
}

// These nonces are random values to make it unlikely that they will appear in memory by accident.
const NONCE_NEW: u32 = 3_254_320_468;
const NONCE_VALID: u32 = 3_033_817_838;
const NONCE_DROPPED: u32 = 2_620_515_550;

impl<T: Copy> Default for CorruptionTesterDefault<T> {
    fn default() -> CorruptionTesterDefault<T> {
        use std::mem::uninitialized;
        let mut tester = unsafe { uninitialized::<CorruptionTesterDefault<T>>() };
        CorruptionTester::init(&mut tester.0, Initializer::Default);
        assert_eq!(tester.0.state(Initializer::Default), State::New);
        tester
    }
}

/// Initialize a `CorruptionTesterUnsafe`.
///
/// `unsafe_default` is like `Default`'s `default` method, but it takes a pointer to a block of
/// uninitialized memory and initializes it to an instance of `CorruptionTesterUnsafe`.
pub unsafe fn unsafe_default<T: Copy>(ptr: *mut CorruptionTesterUnsafe<T>) {
    // The memory we're being given should either be invalid (i.e., not a CorruptionTester) or
    // should be an already-dropped CorruptionTester.
    let state = (*ptr).0.state(Initializer::Unsafe);
    assert!(
        state == State::Invalid || state == State::Dropped,
        "state: {:?}",
        state
    );

    CorruptionTester::init(&mut (*ptr).0, Initializer::Unsafe);
    assert_eq!((*ptr).0.state(Initializer::Unsafe), State::New);
}

impl<T: Copy> Drop for CorruptionTesterDefault<T> {
    fn drop(&mut self) {
        self.0.drop(Initializer::Default);
    }
}

impl<T: Copy> Drop for CorruptionTesterUnsafe<T> {
    fn drop(&mut self) {
        self.0.drop(Initializer::Unsafe);
    }
}

impl<T: Copy> Debug for CorruptionTester<T> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let (hdr, slc) = self.to_header_and_slice();
        write!(f, "{{header: {:?} hash: {:?}}}", hdr, slc)
    }
}

impl<T: Copy> CorruptionTesterWrapper for CorruptionTesterDefault<T> {
    fn state(&self) -> State {
        self.0.state(Initializer::Default)
    }

    fn update_new(&mut self, check_state: bool) {
        self.0.update_new(check_state, Initializer::Default);
    }
}

impl<T: Copy> CorruptionTesterWrapper for CorruptionTesterUnsafe<T> {
    fn state(&self) -> State {
        self.0.state(Initializer::Unsafe)
    }

    fn update_new(&mut self, check_state: bool) {
        self.0.update_new(check_state, Initializer::Unsafe);
    }
}

impl<T: Copy> CorruptionTester<T> {
    /// Initialize a new tester.
    ///
    /// `ptr` points to an uninitialized `CorruptionTester<T>`. If `init` is `UnsafeDefault`, then
    /// `ptr` is taken to be the final location of the tester in memory, and the pointer is used in
    /// computing the hash. Otherwise, `ptr` is simply taken to be a temporary location, and the
    /// pointer is not used in computing the hash.
    fn init(ptr: *mut CorruptionTester<T>, init: Initializer) {
        unsafe {
            {
                use std::ptr::write;
                let (hdr, slc) = (*ptr).to_header_and_slice_mut();
                for elem in slc.iter_mut() {
                    *elem = self::rand::random();
                }
                let ptr_arg = match init {
                    Initializer::Default => 0,
                    Initializer::Unsafe => ptr as usize,
                };
                write(
                    hdr,
                    Header {
                        state_nonce: NONCE_NEW,
                        hash: Self::hash(ptr_arg, NONCE_NEW, slc),
                    },
                );
            }
            assert_eq!((*ptr).state(init), State::New);
        }
    }

    /// Compute the tester's state.
    fn state(&self, init: Initializer) -> State {
        let (hdr, slc) = self.to_header_and_slice();
        if hdr.state_nonce == NONCE_NEW {
            let ptr = match init {
                Initializer::Default => 0,
                Initializer::Unsafe => self as *const CorruptionTester<T> as usize,
            };
            let hash = Self::hash(ptr, hdr.state_nonce, slc);
            if hdr.hash == hash {
                State::New
            } else {
                State::Invalid
            }
        } else {
            let hash = Self::hash(
                self as *const CorruptionTester<T> as usize,
                hdr.state_nonce,
                slc,
            );
            let tuple = (hdr.state_nonce, hdr.hash);
            if tuple == (NONCE_VALID, hash) {
                State::Valid
            } else if tuple == (NONCE_DROPPED, hash) {
                State::Dropped
            } else {
                State::Invalid
            }
        }
    }

    /// Update the tester's state from `New` to `Valid`.
    ///
    /// If `check_state` is true, `update_new` will first verify that the current state is `New`.
    fn update_new(&mut self, check_state: bool, init: Initializer) {
        if check_state {
            assert_eq!(self.state(init), State::New);
        }
        self.update_state(State::Valid);
    }

    /// Update the tester's state.
    ///
    /// `update_state` updates the tester's state by updating the `state_nonce` field and
    /// recomputing the `hash`. `new_state` must be `Valid` or `Dropped`.
    fn update_state(&mut self, new_state: State) {
        let nonce = match new_state {
            State::Valid => NONCE_VALID,
            State::Dropped => NONCE_DROPPED,
            _ => unreachable!(),
        };
        let ptr = self as *mut CorruptionTester<T> as usize;
        let (hdr, slc) = self.to_header_and_slice_mut();
        hdr.state_nonce = nonce;
        hdr.hash = Self::hash(ptr, nonce, slc);
    }

    /// Drop the tester.
    ///
    /// This should only be called by the the `drop` methods of `CorruptionTesterDefault` and
    /// `CorruptionTesterUnsafe`.
    fn drop(&mut self, init: Initializer) {
        use std::thread::panicking;
        if panicking() {
            // At best this function exits cleanly, in which case it didn't help anything. At
            // worst, it causes another panic, in which case we panic while panicking, which aborts
            // the program without a stack trace, which is also unhelpful.
            return;
        }

        assert!(!(self as *mut CorruptionTester<T>).is_null());
        let state = self.state(init);
        if state != State::Valid && state != State::New {
            let addr = self as *mut CorruptionTester<T> as usize;
            panic!(
                "unexpected state {:?} for object {:?} at address {}",
                state, self, addr
            );
        }
        self.update_state(State::Dropped);
        // prevent the optimizer from optimizing away this drop function
        black_box(self);
    }

    /// Split into a header and byte slice.
    fn to_header_and_slice(&self) -> (&Header, &[u8]) {
        use std::mem::{size_of, transmute};
        use std::slice::from_raw_parts;
        let size = size_of::<CorruptionTester<T>>();
        let header_size = size_of::<Header>();

        unsafe {
            let ptr = self as *const CorruptionTester<T> as usize;
            (
                transmute(ptr),
                from_raw_parts((ptr + header_size) as *const u8, size - header_size),
            )
        }
    }

    /// Split into a mutable header and byte slice.
    #[cfg_attr(feature = "cargo-clippy", allow(wrong_self_convention))]
    fn to_header_and_slice_mut(&mut self) -> (&mut Header, &mut [u8]) {
        use std::mem::{size_of, transmute};
        use std::slice::from_raw_parts_mut;
        let size = size_of::<CorruptionTester<T>>();
        let header_size = size_of::<Header>();

        unsafe {
            let ptr = self as *mut CorruptionTester<T> as usize;
            (
                transmute(ptr),
                from_raw_parts_mut((ptr + header_size) as *mut u8, size - header_size),
            )
        }
    }

    /// Hash the pointer, state nonce, and random bytes.
    ///
    /// `hash` computes the xxHash checksum of the concatenation of `ptr`, `state_nonce`, and
    /// `random_bytes` (in that order).
    fn hash(ptr: usize, state_nonce: u32, random_bytes: &[u8]) -> u32 {
        use self::twox_hash::XxHash;
        use std::hash::Hasher;

        // we could do with_seed(0) and then write_usize(ptr), but this is (marginally) faster
        let mut hasher = XxHash::with_seed(ptr as u64);
        hasher.write_u32(state_nonce);
        hasher.write(random_bytes);
        hasher.finish() as u32
    }
}

/// A utility for checking whether a region of memory is mapped.
mod mapped {
    #[cfg(windows)]
    extern crate kernel32;
    #[cfg(unix)]
    extern crate libc;
    #[cfg(windows)]
    extern crate winapi;

    use std::ptr::NonNull;

    #[cfg(unix)]
    lazy_static!{
        static ref RANDOM_FD: i32 = unsafe {
            use std::ffi::CString;
            // NOTE: Both /dev/null and /dev/zero succeed unconditionally, so use /dev/random
            let cstr = CString::new("/dev/random").unwrap();
            let fd = libc::open(cstr.as_ptr(), libc::O_WRONLY);
            assert!(fd >= 0);
            fd
        };
    }

    /// Checks whether a range of memory is mapped.
    ///
    /// Returns true if the range `[ptr, ptr + len)` represents mapped memory.
    #[cfg(unix)]
    pub fn is_mapped_range(ptr: NonNull<u8>, len: usize) -> bool {
        // Credit to http://stackoverflow.com/a/24081504/836390
        unsafe {
            let ret = libc::write(*RANDOM_FD, ptr.cast().as_ptr(), len);
            assert!(ret < 0 || (ret as usize) == len, "ret: {}", ret);
            (ret as usize) == len
        }
    }

    #[cfg(windows)]
    pub fn is_mapped_range(ptr: NonNull<u8>, len: usize) -> bool {
        use self::kernel32::{GetCurrentProcess, K32QueryWorkingSet};
        use self::winapi::shared::basetsd::ULONG_PTR;
        use self::winapi::um::psapi::PSAPI_WORKING_SET_BLOCK;
        use std::mem::{size_of, uninitialized};
        use std::os::raw::c_void;

        // NOTE: winapi 0.3.4 has winapi::um::psapi::PSAPI_WORKING_SET_INFORMATION,
        // but it's defined as having a single entry (just like the equivalent type
        // defined in the Windows docs: https://msdn.microsoft.com/en-us/library/windows/desktop/ms684910(v=vs.85).aspx).
        // In order to support more than a single entry, we'd need to think about how
        // Rust would deal with laying out entries after the end of a struct> Logically,
        // the memory layout would be something like:
        //   (PSAPI_WORKING_SET_INFORMATION, [PSAPI_WORKING_SET_BLOCK])
        // It's easier to just define it here ourselves as having many entries, and
        // thus avoiding having to worry about memory layout issues.

        // NOTE: It's possible that there will be more than 64 * 1024 entries,
        // in which case K32QueryWorkingSet will return ERROR_BAD_LENGTH.
        // If this becomes a problem, we may have to either bump the length
        // of the entries field (while watching out for stack overflow; we
        // could alternatively heap-allocate) or write the logic to dynamically
        // allocate the right number of entries (on error, K32QueryWorkingSet
        // will write the required number of entries into the num_entries field).

        #[repr(C)]
        struct WorkingSets {
            num_entries: ULONG_PTR,
            entries: [PSAPI_WORKING_SET_BLOCK; 64 * 1024],
        }

        let entries = unsafe {
            let mut entries = uninitialized::<WorkingSets>();
            let ptr = &mut entries as *mut _ as *mut c_void;
            let size = size_of::<WorkingSets>() as u32;
            assert_eq!(K32QueryWorkingSet(GetCurrentProcess(), ptr, size), 1);
            entries
        };

        let entries = &entries.entries[..entries.num_entries];

        // See https://msdn.microsoft.com/en-us/library/windows/desktop/ms684902(v=vs.85).aspx
        // for layout of entries.

        // TODO: Now that winapi 0.3 has landed, there is support for accessing the bitfields of
        // PSAPI_WORKING_SET_BLOCK programmatically (see https://github.com/retep998/winapi-rs/issues/482).
        // Iterate over the entries to ensure the entire range is mapped.
        false
    }

    #[cfg(test)]
    mod tests {
        use std::ptr::NonNull;

        #[test]
        fn test_is_mapped_range() {
            use super::is_mapped_range;
            use std::mem::size_of;

            let arr = Box::new([0usize; 100]);
            unsafe {
                let ptr = NonNull::new(Box::into_raw(arr)).unwrap();
                assert!(is_mapped_range(ptr.cast(), size_of::<[usize; 100]>()));
                assert!(!is_mapped_range(
                    NonNull::new(1usize as *mut u8).unwrap(),
                    1
                ));
                Box::from_raw(ptr.as_ptr()); // make sure it gets dropped properly
            }
        }
    }
}

use std::marker::PhantomData;

/// A builder for tests.
///
/// A `TestBuilder` is used to configure and run corruption tests.
pub struct TestBuilder<T, O: ObjectAlloc<T>, F: Fn() -> O> {
    new: F,
    test_iters: usize,
    qc_tests: Option<usize>,
    _marker: PhantomData<(T, O)>,
}

impl<T, O: ObjectAlloc<T>, F: Fn() -> O> TestBuilder<T, O, F> {
    /// Construct a new `TestBuilder`.
    ///
    /// `new` takes a function which, when run, returns an allocator implementing either
    /// `ObjectAlloc<CorruptionTesterDefault>` or `ObjectAlloc<CorruptionTesterUnsafe>`.
    /// `CorruptionTesterDefault` implements `Default`, and so `CorruptionTesterDefault` objects
    /// can be constructed using the `default` method. `CorruptionTesterUnsafe` objects should be
    /// constructed using the `unsafe_default` function defined in this module.
    ///
    /// Regardless of which object type is allocated, the tests that can be run are largely
    /// the same. The only difference is that an allocator of `CorruptionTesterUnsafe` objects
    /// can be tested for a slightly larger set of corruption errors, and so it should be preferred
    /// if possible.
    ///
    /// # `LeakyAlloc`
    ///
    /// If possible, the returned `ObjectAlloc` should use a `LeakyAlloc` to supply its backing
    /// memory. This will make it so that freed backing memory still remains accessible, which
    /// provides this testing framework with a better chance of catching certain types of bugs.
    pub fn new(new: F) -> TestBuilder<T, O, F> {
        TestBuilder {
            new,
            test_iters: 100_000,
            qc_tests: None,
            _marker: PhantomData,
        }
    }

    /// Configure the number of iterations to perform.
    ///
    /// The test run by the `test` method works by repeatedly allocating and deallocating objects.
    /// Each allocation or deallocation is considered an iteration, and this method configures the
    /// number of iterations that are performed. The default is 100,000.
    pub fn test_iters(mut self, iters: usize) -> TestBuilder<T, O, F> {
        self.test_iters = iters;
        self
    }

    /// Configure the number of quickcheck tests to perform.
    ///
    /// The test run by the `quickcheck` method works by constructing a sequence of allocations and
    /// deallocations, and then performing that sequence. This process is repeated over and over
    /// until a failure is found. This method configures the number of such tests that are
    /// performed before the test is considered successful and halted. By default, the `quickcheck`
    /// crate sets a default number of tests to run.
    pub fn quickcheck_tests(mut self, tests: usize) -> TestBuilder<T, O, F> {
        self.qc_tests = Some(tests);
        self
    }
}

impl<T: Copy, O: ObjectAlloc<CorruptionTesterDefault<T>>, F: Fn() -> O>
    TestBuilder<CorruptionTesterDefault<T>, O, F>
where
    T: Send + 'static,
    O: 'static,
    F: Send + Sync + 'static,
{
    pub fn quickcheck(self) {
        self::quickcheck::test(self.new, self.qc_tests)
    }

    pub fn test(self) {
        self.priv_test()
    }
}

impl<T: Copy, O: ObjectAlloc<CorruptionTesterUnsafe<T>>, F: Fn() -> O>
    TestBuilder<CorruptionTesterUnsafe<T>, O, F>
where
    T: Send + 'static,
    O: 'static,
    F: Send + Sync + 'static,
{
    pub fn quickcheck(self) {
        self::quickcheck::test(self.new, self.qc_tests)
    }

    pub fn test(self) {
        self.priv_test()
    }
}

impl<C: CorruptionTesterWrapper, O: ObjectAlloc<C>, F: Fn() -> O> TestBuilder<C, O, F> {
    fn priv_test(self) {
        let mut tester = Tester::new((self.new)());

        for _ in 0..self.test_iters {
            if rand::random() && !tester.alloced.is_empty() {
                tester.dealloc(rand::random());
            } else {
                tester.alloc();
            }
        }

        tester.drop_and_check();
    }
}

mod quickcheck {
    extern crate quickcheck;

    use self::quickcheck::{Arbitrary, Gen, QuickCheck, TestResult, Testable};
    use super::*;
    use std::marker::PhantomData;

    pub fn test<C, O, F>(new: F, tests: Option<usize>)
    where
        C: CorruptionTesterWrapper + Send + 'static,
        O: ObjectAlloc<C> + 'static,
        F: Fn() -> O + Send + Sync + 'static,
    {
        let mut qc = QuickCheck::new();
        if let Some(tests) = tests {
            qc = qc.tests(tests).max_tests(tests);
        }
        qc.quickcheck(TestGenerator(new, PhantomData));
    }

    struct TestGenerator<C, O, F>(F, PhantomData<C>)
    where
        C: CorruptionTesterWrapper,
        O: ObjectAlloc<C>,
        F: Fn() -> O;

    impl<C, O, F> Testable for TestGenerator<C, O, F>
    where
        C: CorruptionTesterWrapper + Send + 'static,
        O: ObjectAlloc<C> + 'static,
        F: Fn() -> O + Send + 'static,
    {
        fn result<G: Gen>(&self, g: &mut G) -> TestResult {
            let ops = Vec::<AllocOp>::arbitrary(g);
            run_alloc_sequence(Tester::new(self.0()), &ops)
        }
    }

    #[derive(Copy, Clone, Debug)]
    pub enum AllocOp {
        Alloc,
        Dealloc(usize),
    }

    impl Arbitrary for AllocOp {
        fn arbitrary<G: Gen>(g: &mut G) -> AllocOp {
            if g.gen() {
                AllocOp::Alloc
            } else {
                AllocOp::Dealloc(g.gen())
            }
        }
    }

    fn run_alloc_sequence<C: CorruptionTesterWrapper, O: ObjectAlloc<C>>(
        mut tester: Tester<C, O>,
        seq: &[AllocOp],
    ) -> TestResult {
        for op in seq {
            match *op {
                AllocOp::Alloc => tester.alloc(),
                AllocOp::Dealloc(idx) => {
                    if idx < tester.alloced.len() {
                        tester.dealloc(idx);
                    } else {
                        tester.drop_and_check();
                        return TestResult::discard();
                    }
                }
            }
        }

        tester.drop_and_check();
        TestResult::passed()
    }
}

use std::collections::HashSet;

struct Tester<C: CorruptionTesterWrapper, O: ObjectAlloc<C>> {
    alloc: O,
    alloced: Vec<NonNull<C>>,
    alloced_set: HashSet<NonNull<C>>,
    freed: HashSet<NonNull<C>>,
}

impl<C: CorruptionTesterWrapper, O: ObjectAlloc<C>> Tester<C, O> {
    fn new(alloc: O) -> Tester<C, O> {
        Tester {
            alloc: alloc,
            alloced: Vec::new(),
            alloced_set: HashSet::new(),
            freed: HashSet::new(),
        }
    }

    fn alloc(&mut self) {
        let obj = unsafe { self.alloc.alloc().unwrap() };
        // check for double-allocate of the same pointer
        assert!(!self.alloced_set.contains(&obj));
        if self.freed.remove(&obj) {
            // We alloced the same pointer at some point in the past and then freed it. It should
            // either be still constructed (Valid) because it hadn't been dropped yet or New
            // because it was dropped and then re-initialized using default() or unsafe_default().
            // Any other state is a bug.
            match unsafe { (*obj.as_ptr()).state() } {
                State::New => unsafe {
                    // we just verified the state, so no sense in wasting time checking again
                    (*obj.as_ptr()).update_new(false);
                },
                State::Valid => {}
                state => {
                    panic!(
                        "newly-allocated object at {:?} in unexpected state {:?}",
                        obj, state
                    );
                }
            }
        } else {
            // this is memory we've never seen before, so it should be New
            match unsafe { (*obj.as_ptr()).state() } {
                State::New => unsafe {
                    // we just verified the state, so no sense in wasting time checking again
                    (*obj.as_ptr()).update_new(false);
                },
                state => {
                    panic!(
                        "newly-allocated object at {:?} in unexpected state {:?}",
                        obj, state
                    );
                }
            }
        }
        self.alloced.push(obj);
        self.alloced_set.insert(obj);
    }

    fn dealloc(&mut self, idx: usize) {
        let len = self.alloced.len();
        let obj: NonNull<C> = self.alloced.swap_remove(idx % len);
        // make sure it's still valid
        assert_eq!(unsafe { (*obj.as_ptr()).state() }, State::Valid);
        self.alloced_set.remove(&obj);
        self.freed.insert(obj);
        unsafe {
            self.alloc.dealloc(obj);
        }
    }

    fn drop_and_check(mut self) {
        use std::mem::drop;

        while !self.alloced.is_empty() {
            let idx = self.alloced.len() - 1;
            self.dealloc(idx);
        }

        let Tester { alloc, freed, .. } = self;

        drop(alloc);
        for obj in freed {
            use std::mem;
            if !mapped::is_mapped_range(obj.cast(), mem::size_of::<C>()) {
                // the underlying memory already got freed back to the kernel
                continue;
            }
            match unsafe { (*obj.as_ptr()).state() } {
                State::Invalid | State::Dropped => {}
                state => {
                    panic!("freed object at {:?} in unexpected state: {:?}", obj, state);
                }
            }
        }
    }
}

#[cfg(test)]
pub mod tests {
    extern crate rand;
    extern crate test;

    use super::*;
    use std::{mem, ptr};

    fn make_default_boxed() -> Box<CorruptionTesterDefault> {
        Box::new(CorruptionTesterDefault::default())
    }

    fn make_unsafe_default_boxed() -> Box<CorruptionTesterUnsafe> {
        unsafe {
            let obj = mem::uninitialized::<CorruptionTesterUnsafe>();
            let mut bx = Box::new(obj);
            use std::ops::DerefMut;
            unsafe_default(bx.deref_mut());
            bx
        }
    }

    fn test_corruption_tester_helper<C: CorruptionTesterWrapper>(mut obj: Box<C>) {
        assert_eq!(obj.state(), State::New);
        obj.update_new(true);
        assert_eq!(obj.state(), State::Valid);

        let ptr = Box::into_raw(obj);
        unsafe {
            // Save the bytes (ptr::read doesn't drop) so we can write them
            // back after dropping in place. If we didn't do this, when we
            // put ptr back into a Box and dropped that Box, the CorruptionTesterWrapper
            // drop implementation would discover it had already been dropped
            // and would panic.
            let bytes = ptr::read(ptr);
            // drop in place (rather than just doing mem::drop(Box::from_raw(ptr)))
            // so that we're guaranteed that the underlying space hasn't been
            // unmapped or overwritten. A previous version of this function just
            // freed and then hoped that the memory would still be valid, and it
            // turned out not to be a valid assumption on Windows.
            ptr::drop_in_place(ptr);
            assert_eq!((*ptr).state(), State::Dropped);
            ptr::write(ptr, bytes);
            Box::from_raw(ptr);
        }
    }

    #[test]
    fn test_corruption_tester() {
        test_corruption_tester_helper(make_default_boxed());
        test_corruption_tester_helper(make_unsafe_default_boxed());
    }

    fn test_corruption_tester_corruption_helper<C: CorruptionTesterWrapper>(
        mut a: Box<C>,
        mut b: Box<C>,
    ) {
        assert_eq!(a.state(), State::New);
        assert_eq!(b.state(), State::New);
        a.update_new(true);
        b.update_new(true);
        assert_eq!(a.state(), State::Valid);
        assert_eq!(b.state(), State::Valid);

        use std::ops::DerefMut;
        unsafe { ptr::copy(a.deref_mut(), b.deref_mut(), 1) };
        assert_eq!(a.state(), State::Valid);
        assert_eq!(b.state(), State::Invalid);
        mem::forget(b); // so it doesn't panic when being dropped
    }

    #[test]
    fn test_corruption_tester_corruption() {
        test_corruption_tester_corruption_helper(make_default_boxed(), make_default_boxed());
        test_corruption_tester_corruption_helper(
            make_unsafe_default_boxed(),
            make_unsafe_default_boxed(),
        );
    }

    fn test_corruption_tester_corruption_panic_on_drop_helper<C: CorruptionTesterWrapper>() {
        use std::mem::zeroed;
        let _tester: C = unsafe { zeroed() };
    }

    #[test]
    #[should_panic]
    fn test_corruption_tester_corruption_panic_on_drop() {
        test_corruption_tester_corruption_panic_on_drop_helper::<CorruptionTesterDefault>();
        test_corruption_tester_corruption_panic_on_drop_helper::<CorruptionTesterUnsafe>();
    }
}
