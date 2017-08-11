pub mod size {
    pub fn choose_size<I: Iterator<Item = usize>, F>(sizes: I,
                                                     unused: F,
                                                     max_objects_per_slab: usize)
                                                     -> usize
        where F: Fn(usize) -> Option<(usize, usize)>
    {
        fn leq(a: f64, b: Option<f64>) -> bool {
            match b {
                None => true,
                Some(b) => a <= b,
            }
        }

        // options are a safer version of the canonical method of starting off with f64::MAX
        let (mut best_size, mut min_unused) = (None, None);
        for size in sizes {
            if let Some((objects, unused)) = unused(size) {
                // TODO: Instead of taking 'unused' to be the amount of space left over at the end
                // of the slab, take this to be all space not used for objects (including space
                // used by the header); this will take better account of used header space. Maybe
                // the two will produce equivalent results, though, since the header size is an
                // affine function of the number of objects (one stack element per object)?
                let unused_per_obj = unused as f64 / objects as f64;
                if objects > max_objects_per_slab {
                    if best_size.is_none() {
                        best_size = Some(size);
                        // don't bother to set min_unused since we're about to break, and we don't
                        // use it at all after this loop
                    }
                    break;
                }
                if leq(unused_per_obj, min_unused) {
                    best_size = Some(size);
                    min_unused = Some(unused_per_obj);
                }
            }
        }

        best_size.expect("no valid slab size found")
    }
}

pub mod stack {
    extern crate alloc;
    use core::{mem, ptr};
    use core::marker::PhantomData;

    /// A manually-allocated stack. The `Stack` object itself is only where the metadata lives; the
    /// data itself lives in memory which is manually allocated by the user.
    #[cfg_attr(feature = "use-packed-ptr-stack", allow(unused))]
    pub struct Stack<T> {
        size: usize,
        _marker: PhantomData<T>,
    }

    impl<T> Stack<T> {
        /// Returns the number of bytes of padding that should be placed between an object with the
        /// given size (in bytes) and the data used for a stack. The object must be aligned to at
        /// least the same alignment as T, or else the value returned may be incorrect.
        pub fn padding_after(size: usize) -> usize {
            use self::alloc::allocator::Layout;
            // NOTE: The Layout alignment isn't used here, so we use 1 because it's guaranteed not
            // to cause from_size_align to return None.
            Layout::from_size_align(size, 1)
                .unwrap()
                .padding_needed_for(mem::align_of::<T>())
        }

        /// Returns the number of bytes of memory that need to be allocated by the user in order to
        /// support a stack of the given size.
        pub fn bytes_for(size: usize) -> usize {
            size * mem::size_of::<T>()
        }

        pub fn new() -> Stack<T> {
            Stack {
                size: 0,
                _marker: PhantomData,
            }
        }

        /// Pushes a new element onto the stack.
        ///
        /// The new size of the stack must not exceed the size for which the underlying memory
        /// (`data`) was allocated.
        pub fn push(&mut self, data: *mut T, val: T) {
            Self::set_at_idx(data, self.size as isize, val);
            self.size += 1;
        }

        /// Pops an element from the stack.
        ///
        /// The stack must not be empty.
        pub fn pop(&mut self, data: *mut T) -> T {
            debug_assert!(self.size > 0);
            self.size -= 1;
            Self::get_at_idx(data, self.size as isize)
        }

        /// Returns the current number of elements in the stack.
        pub fn size(&self) -> usize {
            self.size
        }

        #[cfg_attr(feature = "cargo-clippy", allow(inline_always))]
        #[inline(always)]
        fn get_at_idx(data: *mut T, idx: isize) -> T {
            debug_assert!(idx >= 0);
            unsafe { ptr::read(data.offset(idx)) }
        }

        #[cfg_attr(feature = "cargo-clippy", allow(inline_always))]
        #[inline(always)]
        fn set_at_idx(data: *mut T, idx: isize, val: T) {
            debug_assert!(idx >= 0);
            unsafe {
                ptr::write(data.offset(idx), val);
            }
        }
    }
}

pub mod color {
    #[cfg(not(feature = "no-coloring"))]
    pub use self::on::ColorSettings;
    #[cfg(not(feature = "no-coloring"))]
    pub use self::on::Color;

    #[cfg(feature = "no-coloring")]
    pub use self::off::ColorSettings;
    #[cfg(feature = "no-coloring")]
    pub use self::off::Color;

    /// An implementation of the coloring scheme described in Section 4 of [The Slab Allocator: An
    /// Object-Caching Kernel Memory Allocator][1].
    ///
    /// [1]: http://www.usenix.org/publications/library/proceedings/bos94/full_papers/bonwick.ps
    #[cfg(not(feature = "no-coloring"))]
    mod on {
        #[derive(Debug, Copy, Clone)]
        pub struct ColorSettings {
            max_color: usize,
            cur_color: usize,
        }

        impl ColorSettings {
            pub fn new(align: usize, unused: usize) -> ColorSettings {
                ColorSettings {
                    max_color: (unused / align) * align,
                    cur_color: 0,
                }
            }

            pub fn next_color(&mut self, align: usize) -> Color {
                debug_assert_eq!(self.cur_color % align, 0);
                debug_assert_eq!(self.max_color % align, 0);

                let color = self.cur_color;
                assert!(color <= self.max_color);
                self.cur_color = if self.cur_color == self.max_color {
                    0
                } else {
                    self.cur_color + align
                };
                Color(color)
            }

            pub fn max_color(&self) -> Color {
                Color(self.max_color)
            }
        }

        #[derive(Clone, Copy)]
        pub struct Color(usize);

        impl Color {
            pub fn as_usize(&self) -> usize {
                self.0
            }
        }
    }

    #[cfg(feature = "no-coloring")]
    mod off {
        #[derive(Debug, Copy, Clone)]
        pub struct ColorSettings;

        impl ColorSettings {
            pub fn new(_align: usize, _unused: usize) -> ColorSettings {
                ColorSettings {}
            }

            pub fn next_color(&mut self, _align: usize) -> Color {
                Color {}
            }

            pub fn max_color(&self) -> Color {
                Color {}
            }
        }

        #[derive(Clone, Copy)]
        pub struct Color;

        impl Color {
            pub fn as_usize(&self) -> usize {
                0
            }
        }
    }
}

pub mod ptrmap {
    #[cfg(not(feature = "use-stdlib-hashmap"))]
    pub use self::optimized::Map;

    #[cfg(feature = "use-stdlib-hashmap")]
    pub use self::stdlib::Map;

    #[cfg(not(feature = "use-stdlib-hashmap"))]
    mod optimized {
        use ptr_map::PtrHashMap;

        pub struct Map<K, V> {
            pub map: PtrHashMap<K, V>,
        }

        impl<K, V> Map<K, V> {
            pub fn new(size_hint: usize, align: usize) -> Map<K, V> {
                Map { map: PtrHashMap::new(size_hint, align) }
            }

            pub fn get(&self, k: *mut K) -> *mut V {
                self.map.get(k)
            }

            pub fn insert(&mut self, k: *mut K, v: *mut V) {
                self.map.insert(k, v);
            }

            pub fn delete(&mut self, k: *mut K) {
                self.map.delete(k);
            }
        }
    }

    #[cfg(feature = "use-stdlib-hashmap")]
    mod stdlib {
        use std::collections::HashMap;

        pub struct Map<K, V> {
            map: HashMap<*mut K, *mut V>,
        }

        impl<K, V> Map<K, V> {
            pub fn new(size_hint: usize, _: usize) -> Map<K, V> {
                Map { map: HashMap::with_capacity(size_hint) }
            }

            pub fn get(&self, k: *mut K) -> *mut V {
                *self.map.get(&k).unwrap()
            }

            pub fn insert(&mut self, k: *mut K, v: *mut V) {
                self.map.insert(k, v);
            }

            pub fn delete(&mut self, k: *mut K) {
                self.map.remove(&k);
            }
        }
    }
}

pub mod list {
    extern crate core;
    use core::ptr;

    pub trait Linkable {
        fn next(&self) -> *mut Self;
        fn prev(&self) -> *mut Self;
        fn set_next(&mut self, ptr: *mut Self);
        fn set_prev(&mut self, ptr: *mut Self);
    }

    pub struct LinkedList<T: Linkable> {
        size: usize,
        head: *mut T,
        tail: *mut T,
    }

    impl<T: Linkable> LinkedList<T> {
        pub fn new() -> LinkedList<T> {
            LinkedList {
                size: 0,
                head: ptr::null_mut(),
                tail: ptr::null_mut(),
            }
        }

        pub fn insert_front(&mut self, t: *mut T) {
            unsafe {
                debug_assert!((*t).next().is_null());
                debug_assert!((*t).prev().is_null());
                if self.size == 0 {
                    self.head = t;
                    self.tail = t;
                } else {
                    (*t).set_next(self.head);
                    (*self.head).set_prev(t);
                    self.head = t;
                }
                self.size += 1;
            }
        }

        pub fn insert_back(&mut self, t: *mut T) {
            unsafe {
                debug_assert!((*t).next().is_null());
                debug_assert!((*t).prev().is_null());
                if self.size == 0 {
                    self.head = t;
                    self.tail = t;
                } else {
                    (*self.tail).set_next(t);
                    (*t).set_prev(self.tail);
                    self.tail = t;
                }
                self.size += 1;
            }
        }

        pub fn remove_front(&mut self) -> *mut T {
            unsafe {
                debug_assert!(self.size > 0);
                let t = if self.size == 1 {
                    let t = self.head;
                    self.head = ptr::null_mut();
                    self.tail = ptr::null_mut();
                    t
                } else {
                    let t = self.head;
                    self.head = (*t).next();
                    (*self.head).set_prev(ptr::null_mut());
                    (*t).set_next(ptr::null_mut());
                    t
                };
                debug_assert!((*t).next().is_null());
                debug_assert!((*t).prev().is_null());
                self.size -= 1;
                t
            }
        }

        pub fn remove_back(&mut self) -> *mut T {
            unsafe {
                debug_assert!(self.size > 0);
                let t = if self.size == 1 {
                    let t = self.head;
                    self.head = ptr::null_mut();
                    self.tail = ptr::null_mut();
                    t
                } else {
                    let t = self.tail;
                    self.tail = (*t).prev();
                    (*self.tail).set_next(ptr::null_mut());
                    (*t).set_prev(ptr::null_mut());
                    t
                };
                debug_assert!((*t).next().is_null());
                debug_assert!((*t).prev().is_null());
                self.size -= 1;
                t
            }
        }

        pub fn move_to_back(&mut self, t: *mut T) {
            debug_assert!(self.size > 0);
            if self.size == 1 || self.tail == t {
                return;
            }

            unsafe {
                // remove from its place in the list
                if self.head == t {
                    let next = (*t).next();
                    (*next).set_prev(ptr::null_mut());
                    self.head = next;
                } else {
                    let prev = (*t).prev();
                    let next = (*t).next();
                    (*prev).set_next(next);
                    (*next).set_prev(prev);
                }

                // insert at the back of the list
                (*t).set_prev(self.tail);
                (*t).set_next(ptr::null_mut());
                (*self.tail).set_next(t);
                self.tail = t;
            }
        }

        pub fn peek_front(&self) -> *mut T {
            debug_assert!(self.size > 0);
            self.head
        }

        pub fn size(&self) -> usize {
            self.size
        }
    }
}

pub mod workingset {
    use std::time::Instant;

    pub struct WorkingSet<T: Copy> {
        data: T,
        period_begin: Instant,
    }

    impl<T: Copy> WorkingSet<T> {
        pub fn new(init: T) -> WorkingSet<T> {
            WorkingSet {
                data: init,
                period_begin: Instant::now(),
            }
        }

        pub fn set(&mut self, new: T) {
            self.data = new;
        }

        /// Refreshes the working set.
        ///
        /// If at least `secs` seconds have elapsed since the beginning of the period, the period
        /// is reset and the current value is returned. Otherwise, `refresh` returns `None` and is
        /// a no-op. Note that unless `set` is called after `refresh`, the stored `T` value will be
        /// the same in the new period.
        pub fn refresh(&mut self, secs: u64) -> Option<T> {
            self.refresh_now(secs, Instant::now())
        }

        /// Refreshes the working set given the current time.
        ///
        /// `refresh_now` is like `refresh`, but it takes the current time as an argument instead
        /// of looking it up.
        pub fn refresh_now(&mut self, secs: u64, now: Instant) -> Option<T> {
            if now.duration_since(self.period_begin).as_secs() >= secs {
                self.period_begin = now;
                Some(self.data)
            } else {
                None
            }
        }
    }

    impl<T: Ord + Copy> WorkingSet<T> {
        /// Updates the running minimum.
        ///
        /// `update_min` updates the running minimum value if `new` is less than the current
        /// minimum.
        pub fn update_min(&mut self, new: T) {
            if new < self.data {
                self.data = new;
            }
        }
    }
}

pub mod misc {
    extern crate alloc;
    use self::alloc::allocator::Layout;

    pub fn satisfy_min_align(layout: Layout, min_align: usize) -> Layout {
        if min_align <= layout.align() {
            layout
        } else if layout.size() % min_align == 0 {
            layout.align_to(min_align)
        } else {
            let remainder = layout.size() % min_align;
            Layout::from_size_align(layout.size() + min_align - remainder, min_align).unwrap()
        }
    }
}
