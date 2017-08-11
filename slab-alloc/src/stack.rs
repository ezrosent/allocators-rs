// So clippy doesn't complain that IllumOS isn't in tick marks
#![cfg_attr(feature = "cargo-clippy", allow(doc_markdown))]
//! A generic stack-based slab.
//!
//! This module implements a generic slab which uses an inline header and a stack of object
//! pointers in place of a free list. It differs from both the large and small slab algorithms
//! introduced by Bonwick. Bonwick's large slabs used a separately-allocated header and a free list
//! of separately-allocated control objects (`kmem_bufctl`s in the IllumOS implementation).
//! Bonwick's small slabs used an inline header like the present implementation, but used a free
//! list constructed from headers on each object instead of a stack of pointers.
//!
//! This implementation is generic in that it does not prescribe a method for mapping objects to
//! their containing slabs, but instead requires that an implementation of this functionality be
//! provided (see the `ConfigData` trait). The `aligned` module implements this functionality by
//! ensuring that slabs have an alignment equal to their size, and using this to compute a bit mask
//! for objects in the slab. The `large` module implements this functionality by storing object
//! pointers or page addresses in an allocator-global hash table.
//!
//! # Layout
//! The layout of stack-based slabs is somewhat confusing, and not readily obvious from the code.
//! This is due largely to the fact that slab size cannot be known at compile time, and must
//! instead be computed at runtime. Why this is a problem will become apparent shortly.
//!
//! The layout in memory of stack-based slabs is as follows:
//!
//! ```text
//! <header> <pre-stack padding> <stack> <post-stack padding> <array of objects>
//! ```
//!
//! The following requirements must be met with respect to memory layout:
//!
//! * The stack - which is an array of `usize` - must be aligned according to the alignment
//!   required by `usize`
//! * The array of objects must be aligned according to the alignment requested by the user.
//!
//! The first requirement implies that there may need to be some padding between the header and the
//! stack. The second requirement implies that there may need to be some padding between the stack
//! and the array of objects.
//!
//! If the number of objects in a slab could be known statically, the stack could simply be an
//! array in the header. Instead, its size has to be computed dynamically, and thus cannot be a
//! field in the header (it could technically be `[*mut T]`, but this would make querying the
//! header's size more difficult).
//!
//! Instead, we use the `util::stack` module to implement a dynamically-sized stack, and to
//! dynamically compute the proper pre-stack padding required in order to give the stack the proper
//! alignment. We do the same for the post-stack padding in order to give the array of objects the
//! proper alignment.

extern crate alloc;
extern crate object_alloc;

use SlabSystem;
use init::InitSystem;
use core::{mem, ptr};
use util::stack::Stack;
use util::color::{ColorSettings, Color};
use util::list::*;
use self::alloc::allocator;
use self::object_alloc::UntypedObjectAlloc;

/// Configuration to customize a stack-based slab implementation.
///
/// `ConfigData` completes the stack-based slab implementation by providing post-alloc and
/// pre-dealloc hooks and by providing a mechanism to look up an object's containing slab.
pub trait ConfigData
    where Self: Sized
{
    /// Perform per-slab post-allocation work.
    ///
    /// `post_alloc` is called after a newly-allocated slab has been initialized. It is optional,
    /// and defaults to a no-op.
    #[allow(unused)]
    fn post_alloc(&mut self, layout: &Layout, slab_size: usize, slab: *mut SlabHeader) {}

    /// Perform per-slab pre-deallocation work.
    ///
    /// `pre_dealloc` is called before a slab is uninitialized and deallocated. It is optional, and
    /// defaults to a no-op.
    #[allow(unused)]
    fn pre_dealloc(&mut self, layout: &Layout, slab_size: usize, slab: *mut SlabHeader) {}

    /// Look up an object's slab.
    ///
    /// Given an object, `ptr_to_slab` locates the slab containing that object.
    fn ptr_to_slab(&self, slab_size: usize, ptr: *mut u8) -> *mut SlabHeader;
}

pub struct System<A: UntypedObjectAlloc, C: ConfigData> {
    pub data: C,
    layout: Layout,
    alloc: A,
}

impl<A: UntypedObjectAlloc, C: ConfigData> System<A, C> {
    pub fn from_config_data(data: C, layout: Layout, alloc: A) -> System<A, C> {
        System {
            data: data,
            layout: layout,
            alloc: alloc,
        }
    }
}

impl<I: InitSystem, A: UntypedObjectAlloc, C: ConfigData> SlabSystem<I> for System<A, C> {
    type Slab = SlabHeader;

    fn alloc_slab(&mut self) -> *mut SlabHeader {
        unsafe {
            let color = self.layout
                .color_settings
                .next_color(self.layout.layout.align());
            let slab = match self.alloc.alloc() {
                Ok(slab) => slab as *mut SlabHeader,
                Err(..) => return ptr::null_mut(),
            };

            ptr::write(slab,
                       SlabHeader {
                           stack: Stack::new(),
                           color: color,
                           next: ptr::null_mut(),
                           prev: ptr::null_mut(),
                       });
            let stack_data_ptr = self.layout.stack_begin(slab);
            for i in 0..self.layout.num_obj {
                let ptr = self.layout.nth_obj(slab, color, i);
                (*slab)
                    .stack
                    .push(stack_data_ptr, I::pack(ptr, I::status_uninitialized()));
            }

            self.data
                .post_alloc(&self.layout, self.alloc.layout().size(), slab);
            slab
        }
    }

    fn dealloc_slab(&mut self, slab: *mut SlabHeader) {
        unsafe {
            debug_assert_eq!((*slab).stack.size(), self.layout.num_obj);
            self.data
                .pre_dealloc(&self.layout, self.alloc.layout().size(), slab);
            let stack_data_ptr = self.layout.stack_begin(slab);
            for _ in 0..self.layout.num_obj {
                let packed = (*slab).stack.pop(stack_data_ptr);
                I::drop(I::unpack_ptr(packed), I::unpack_status(packed));
            }

            self.alloc.dealloc(slab as *mut u8);
        }
    }

    fn is_full(&self, slab: *mut SlabHeader) -> bool {
        unsafe { (*slab).stack.size() == self.layout.num_obj }
    }

    fn is_empty(&self, slab: *mut SlabHeader) -> bool {
        unsafe { (*slab).stack.size() == 0 }
    }

    fn alloc(&self, slab: *mut SlabHeader) -> (*mut u8, I::Status) {
        unsafe {
            let stack_data_ptr = self.layout.stack_begin(slab);
            let packed = (*slab).stack.pop(stack_data_ptr);
            (I::unpack_ptr(packed), I::unpack_status(packed))
        }
    }

    fn dealloc(&self, obj: *mut u8, init_status: I::Status) -> (*mut SlabHeader, bool) {
        unsafe {
            let slab = self.data.ptr_to_slab(self.alloc.layout().size(), obj);
            let was_empty = (*slab).stack.size() == 0;

            let stack_data_ptr = self.layout.stack_begin(slab);
            (*slab)
                .stack
                .push(stack_data_ptr, I::pack(obj, init_status));
            (slab, was_empty)
        }
    }
}

pub struct SlabHeader {
    stack: Stack<usize>, // note: this is only the metadata; the real stack comes after this header
    color: Color, // extra padding added before array beginning
    next: *mut SlabHeader,
    prev: *mut SlabHeader,
}

impl Linkable for SlabHeader {
    fn next(&self) -> *mut SlabHeader {
        self.next
    }
    fn prev(&self) -> *mut SlabHeader {
        self.prev
    }
    fn set_next(&mut self, next: *mut SlabHeader) {
        self.next = next;
    }
    fn set_prev(&mut self, prev: *mut SlabHeader) {
        self.prev = prev;
    }
}

impl SlabHeader {
    pub fn get_color(&self) -> Color {
        self.color
    }
}

#[derive(Clone)]
pub struct Layout {
    pub num_obj: usize,
    pub layout: allocator::Layout,
    pub stack_begin_offset: usize,
    pub array_begin_offset: usize,
    pub color_settings: ColorSettings,
}

impl Layout {
    /// Determines whether an allocator can be constructed for T using the given slab size. If so,
    /// it returns a constructed Layout for T using that slab size and the amount of unused space
    /// left at the end of the slab (when no coloring is used).
    pub fn for_slab_size(layout: allocator::Layout, slab_size: usize) -> Option<(Layout, usize)> {
        let obj_size = layout.size();
        let obj_align = layout.align();
        let hdr_size = mem::size_of::<SlabHeader>();

        // padding between the SlabHeader and the base of the pointer stack
        let pre_stack_padding = Stack::<usize>::padding_after(hdr_size);
        let stack_begin_offset = hdr_size + pre_stack_padding;

        // Find the largest number of objects we can fit in the slab. array_begin_offset is the
        // offset from the beginning of the slab of the array of objects.
        let (mut num_obj, mut array_begin_offset) = (0, 0);
        loop {
            let candidate = num_obj + 1;
            // total_hdr_size = size of header, post-header padding, and stack
            let total_hdr_size = stack_begin_offset + Stack::<usize>::bytes_for(candidate);

            // padding between the pointer stack and the array of objects
            use self::alloc::allocator::Layout;
            // NOTE: The Layout alignment isn't used here, so we use 1 because it's guaranteed not
            // to cause from_size_align to return None.
            let post_stack_padding = Layout::from_size_align(total_hdr_size, 1)
                .unwrap()
                .padding_needed_for(obj_align);

            if total_hdr_size + post_stack_padding + (candidate * obj_size) <= slab_size {
                num_obj = candidate;
                array_begin_offset = total_hdr_size + post_stack_padding;
            } else {
                break;
            }
        }
        if num_obj == 0 {
            return None;
        }
        assert!(array_begin_offset > 0);

        let unused_space = slab_size - array_begin_offset - (num_obj * obj_size);
        let l = Layout {
            num_obj: num_obj,
            layout: layout,
            stack_begin_offset: stack_begin_offset,
            array_begin_offset: array_begin_offset,
            color_settings: ColorSettings::new(obj_align, unused_space),
        };

        // assert that the objects fit within the slab
        assert!(slab_size >=
                l.array_begin_offset + l.color_settings.max_color().as_usize() +
                (l.num_obj * obj_size));
        Some((l, unused_space))
    }

    fn array_begin(&self, slab: *mut SlabHeader, color: Color) -> *mut u8 {
        debug_assert!(color.as_usize() <= self.color_settings.max_color().as_usize());
        ((slab as usize) + self.array_begin_offset + color.as_usize()) as *mut u8
    }

    fn stack_begin(&self, slab: *mut SlabHeader) -> *mut usize {
        ((slab as usize) + self.stack_begin_offset) as *mut usize
    }

    pub fn nth_obj(&self, slab: *mut SlabHeader, color: Color, n: usize) -> *mut u8 {
        debug_assert!((n as usize) < self.num_obj);
        (self.array_begin(slab, color) as usize + n * self.layout.size()) as *mut u8
    }
}
