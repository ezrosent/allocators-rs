use core::marker::PhantomData;

/// A system to perform initialization of objects.
///
/// Since slab allocators can either initialize their objects or not, we need a way of abstracting
/// over the logic of keeping track of whether objects are initialized that can be completely
/// optimized away if no initialization (and thus no tracking) is actually being done. `InitSystem`
/// abstracts over the logic of keeping track of whether an object has been initialized and of
/// initializing uninitialized objects.
pub trait InitSystem {
    /// The initialization status of an object.
    ///
    /// For implementations that perform initialization, this is a type which keeps track of two
    /// states - initialized or uninitialized. For implementations that do not perform
    /// initialization, this is a ZST - it has only one state, and can be completely optimized out.
    type Status: Copy;

    fn status_initialized() -> Self::Status;
    fn status_uninitialized() -> Self::Status;

    /// The minimum alignment required to pack a status into a pointer.
    ///
    /// All pointers used with this object must guarantee this alignment, or else the behavior will
    /// be undefined.
    fn min_align() -> usize;

    /// Pack a `Status` and a pointer.
    ///
    /// `pack` packs a pointer and a status into a `usize`. `ptr` must be aligned to `min_align()`,
    /// or else the behavior of `pack` is undefined.
    fn pack(ptr: *mut u8, status: Self::Status) -> usize;

    /// Unpack a `Status` from a packed `usize`.
    ///
    /// `unpack_status` unpacks a `Status` from `packed`. It should only be called on values that
    /// were previously returned from `pack`.
    fn unpack_status(packed: usize) -> Self::Status;

    /// Unpack a pointer from a packed `usize`.
    ///
    /// `unpack_ptr` unpacks a pointer from `packed`. It should only be called on values that were
    /// previously returned from `pack`.
    fn unpack_ptr(packed: usize) -> *mut u8;

    /// Initialize an object.
    ///
    /// For implementations that perform initialization, `init` initializes `obj` if it is
    /// currently uninitialized. For implementations that do not perform initialization, `init` is
    /// a no-op.
    fn init(&self, obj: *mut u8, init_status: Self::Status);

    /// Drop an object.
    ///
    /// For implementations that perform initialization, `drop` drops `obj` if it is currently
    /// initialized. For implementations that do not perform initialization, `drop` is a no-op.
    fn drop(obj: *mut u8, init_status: Self::Status);
}

pub struct NopInitSystem;

impl InitSystem for NopInitSystem {
    type Status = ();

    fn status_initialized() -> () {
        ()
    }
    fn status_uninitialized() -> () {
        ()
    }
    fn min_align() -> usize {
        1
    }
    fn pack(ptr: *mut u8, _status: ()) -> usize {
        ptr as usize
    }
    fn unpack_status(_packed: usize) -> () {
        ()
    }
    fn unpack_ptr(packed: usize) -> *mut u8 {
        packed as *mut u8
    }
    fn init(&self, _obj: *mut u8, _init_status: ()) {}
    fn drop(_obj: *mut u8, _init_status: ()) {}
}

pub struct InitInitSystem<T, I: Initializer<T>> {
    init: I,
    _marker: PhantomData<T>,
}

impl<T, I: Initializer<T>> InitInitSystem<T, I> {
    pub fn new(init: I) -> InitInitSystem<T, I> {
        InitInitSystem {
            init: init,
            _marker: PhantomData,
        }
    }
}

impl<T, I: Initializer<T>> InitSystem for InitInitSystem<T, I> {
    type Status = bool;

    fn status_initialized() -> bool {
        true
    }
    fn status_uninitialized() -> bool {
        false
    }
    fn min_align() -> usize {
        2
    }

    fn pack(ptr: *mut u8, status: bool) -> usize {
        (ptr as usize) | if status { 1 } else { 0 }
    }

    fn unpack_status(packed: usize) -> bool {
        packed & 1 == 1
    }

    fn unpack_ptr(packed: usize) -> *mut u8 {
        (packed & (<usize>::max_value() - 1)) as *mut u8
    }

    fn init(&self, obj: *mut u8, init: bool) {
        if !init {
            unsafe {
                self.init.init(obj as *mut T);
            }
        }
    }

    fn drop(obj: *mut u8, init: bool) {
        if init {
            unsafe {
                use core::ptr::drop_in_place;
                drop_in_place(obj as *mut T);
            }
        }
    }
}

pub unsafe trait Initializer<T> {
    unsafe fn init(&self, ptr: *mut T);
}

#[derive(Default)]
pub struct DefaultInitializer<T: Default> {
    _marker: PhantomData<T>,
}

impl<T: Default> DefaultInitializer<T> {
    pub fn new() -> DefaultInitializer<T> {
        DefaultInitializer { _marker: PhantomData }
    }
}

unsafe impl<T: Default> Initializer<T> for DefaultInitializer<T> {
    unsafe fn init(&self, ptr: *mut T) {
        use core::ptr::write;
        write(ptr, T::default());
    }
}

pub struct FnInitializer<T, F: Fn() -> T>(F);

impl<T, F: Fn() -> T> FnInitializer<T, F> {
    pub fn new(f: F) -> FnInitializer<T, F> {
        FnInitializer(f)
    }
}

unsafe impl<T, F: Fn() -> T> Initializer<T> for FnInitializer<T, F> {
    unsafe fn init(&self, ptr: *mut T) {
        use core::ptr::write;
        write(ptr, (self.0)());
    }
}

pub struct UnsafeFnInitializer<T, F: Fn(*mut T)>(F, PhantomData<T>);

impl<T, F: Fn(*mut T)> UnsafeFnInitializer<T, F> {
    pub fn new(f: F) -> UnsafeFnInitializer<T, F> {
        UnsafeFnInitializer(f, PhantomData)
    }
}

unsafe impl<T, F: Fn(*mut T)> Initializer<T> for UnsafeFnInitializer<T, F> {
    unsafe fn init(&self, ptr: *mut T) {
        (self.0)(ptr);
    }
}
