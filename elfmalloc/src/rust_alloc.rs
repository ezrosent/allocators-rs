//! This module is an attempt at craeting a variant of elfmalloc that takes advantage of the extra
//! information that the `Alloc` trait provides.

extern crate alloc;
use self::alloc::allocator::{Alloc, AllocErr, Layout};

use super::general::{Multiples, PowersOfTwo, ObjectAlloc, MULTIPLE, AllocMap};
use super::slag::{PageAlloc, MemorySource, Metadata, RevocablePipe, compute_metadata,
                  CoarseAllocator};
use super::utils::{mmap, Lazy};

use std::cmp;
use std::mem;
use std::ptr;


/// A `MemorySource` that just calls mmap. It still maintains that all pages returned by `carve`
/// are aligned to their size.
#[derive(Copy, Clone)]
struct MmapSource {
    page_size: usize,
}

impl MemorySource for MmapSource {
    fn new(page_size: usize) -> MmapSource {
        MmapSource { page_size: page_size.next_power_of_two() }
    }
    fn page_size(&self) -> usize {
        self.page_size
    }

    fn carve(&self, npages: usize) -> Option<*mut u8> {
        // faster mod for power-of-2 sizes.
        fn mod_size(x: usize, n: usize) -> usize {
            x & (n - 1)
        }

        // There is a faster path available when our local page size is less than or equal to the
        // system one.
        let system_page_size = mmap::page_size();
        if self.page_size <= system_page_size {
            return mmap::fallible_map(npages * self.page_size);
        }
        // We want to return pages aligned to our page size, which is larger than the
        // system page size. As a result, we want to allocate an extra page to guarantee a slice of
        // the memory that is aligned to the larger page size.
        let target_size = npages * self.page_size;
        let req_size = target_size + self.page_size;
        mmap::fallible_map(req_size).and_then(|mem| {
            let mem_num = mem as usize;

            debug_assert_eq!(mod_size(mem_num, system_page_size), 0);

            // region at the end that is not needed.
            let rem1 = mod_size(mem_num, self.page_size);
            // region at the beginning that is not needed.
            let rem2 = self.page_size - rem1;
            unsafe {
                let res = mem.offset(rem2 as isize);
                debug_assert_eq!(mod_size(res as usize, self.page_size), 0);
                if rem1 > 0 {
                    mmap::unmap(res.offset(target_size as isize), rem1);
                }
                if rem2 > 0 {
                    mmap::unmap(mem, rem2);
                }
                Some(res)
            }
        })
    }
}


/// An `Alloc` impl that takes advantage of size information at the call-site to more efficiently
/// handle larger allocations.
#[derive(Clone)]
pub struct ElfMalloc<M: MemorySource> {
    small: Multiples<ObjectAlloc<PageAlloc<M>>>,
    large: PowersOfTwo<Lazy<PageAlloc<M>>>,
}

unsafe impl<M: MemorySource> Send for ElfMalloc<M> {}

macro_rules! case_analyze {
    ($self:expr, $layout:expr, small $small:expr; medium $medium:expr; large $large:expr;) => {
        {
            if $layout.size() <= $self.small.max_key() {
                $small
            } else if $layout.align() <= $self.large.max_key() {
                $medium
            } else {
                $large
            }
        }
    }
}

unsafe impl<M: MemorySource> Alloc for ElfMalloc<M> {
    unsafe fn alloc(&mut self, l: Layout) -> Result<*mut u8, AllocErr> {
        case_analyze!(
            self,
            l,
            small Ok(
                self.small
                    .get_mut(if l.align() > mem::size_of::<usize>() {
                        l.size().next_power_of_two()
                    } else {
                        l.size()
                    })
                    .alloc(),
            );
            medium Ok(self.large.get_mut(l.size()).alloc());
            large match mmap::fallible_map(l.size()) {
                Some(p) => Ok(p),
                None => Err(AllocErr::Exhausted { request: l }),
            };
        )
    }

    unsafe fn dealloc(&mut self, item: *mut u8, l: Layout) {
        case_analyze!(
            self,
            l,
            small self.small.get_mut(if l.align() > mem::size_of::<usize>() {
                        l.size().next_power_of_two()
                    } else {
                        // round up to nearest MULTIPLE
                        (l.size() + (MULTIPLE - 1)) & !(MULTIPLE - 1)
                    }).free(item);
            medium self.large.get_mut(l.size()).free(item, false);
            large mmap::unmap(item, l.size());)
    }

    fn usable_size(&self, l: &Layout) -> (usize, usize) {
        (
            l.size(),
            case_analyze!(
            self,
            l,
            small if l.align() > mem::size_of::<usize>() {
                l.size().next_power_of_two()
            } else {
                l.size()
            };
            medium l.size().next_power_of_two();
            large l.size();),
        )
    }
}

pub struct ElfMallocBuilder {
    page_size: usize,
    target_pa_size: usize,
    reuse_threshold: f64,
    max_object_size: usize,
    small_pipe_size: usize,
    large_pipe_size: usize,
}

impl Default for ElfMallocBuilder {
    fn default() -> ElfMallocBuilder {
        ElfMallocBuilder {
            page_size: 32 << 10,
            // very large -- this effectively turns the feature off
            target_pa_size: 1 << 30,
            reuse_threshold: 0.6,
            max_object_size: 2 << 20,
            small_pipe_size: 8,
            large_pipe_size: 16,
        }
    }
}

impl ElfMallocBuilder {
    pub fn page_size(&mut self, page_size: usize) -> &mut ElfMallocBuilder {
        self.page_size = cmp::max(mmap::page_size(), page_size);
        self
    }
    pub fn target_pa_size(&mut self, target_pa_size: usize) -> &mut ElfMallocBuilder {
        self.target_pa_size = target_pa_size;
        self
    }
    pub fn reuse_threshold(&mut self, reuse_threshold: f64) -> &mut ElfMallocBuilder {
        self.reuse_threshold = reuse_threshold;
        self
    }
    pub fn max_object_size(&mut self, max_object_size: usize) -> &mut ElfMallocBuilder {
        self.max_object_size = max_object_size;
        self
    }
    pub fn small_pipe_size(&mut self, small_pipe_size: usize) -> &mut ElfMallocBuilder {
        self.small_pipe_size = small_pipe_size;
        self
    }
    pub fn large_pipe_size(&mut self, large_pipe_size: usize) -> &mut ElfMallocBuilder {
        self.large_pipe_size = large_pipe_size;
        self
    }

    pub fn build<M: MemorySource>(&self) -> ElfMalloc<M> {
        let pa = PageAlloc::<M>::new(self.page_size, self.target_pa_size);
        let n_small_classes = (self.page_size / 4) / MULTIPLE;
        assert!(n_small_classes > 0);
        let mut meta_pointers = mmap::map(mem::size_of::<Metadata>() * n_small_classes) as
            *mut Metadata;
        let small_classes = Multiples::init(MULTIPLE, n_small_classes, |size: usize| {
            let meta = meta_pointers;
            unsafe {
                meta_pointers = meta_pointers.offset(1);
                ptr::write(
                    meta,
                    compute_metadata(
                        size,
                        self.page_size,
                        0,
                        self.reuse_threshold,
                        self.page_size,
                    ),
                );
            }
            let params = (
                meta,
                usize::max_value(), /* no eager decommit */
                pa.clone(),
                RevocablePipe::new_size(self.small_pipe_size),
            );
            ObjectAlloc::new(params)
        });
        let next_size_class = (small_classes.max_key() + 1).next_power_of_two();
        let max_size = self.max_object_size.next_power_of_two();
        let n_classes = max_size.trailing_zeros() - next_size_class.trailing_zeros();
        let large_classes = PowersOfTwo::init(next_size_class, n_classes as usize, |size: usize| {
            Lazy::<PageAlloc<M>>::new((size, self.target_pa_size))
        });
        debug_assert!(small_classes.max_key().is_power_of_two());
        ElfMalloc {
            small: small_classes,
            large: large_classes,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::mem;
    use std::ptr;
    use std::thread;

    #[test]
    fn basic_alloc_functionality() {
        let word_size = mem::size_of::<usize>();
        let mut alloc = ElfMallocBuilder::default().build::<MmapSource>();
        let layouts: Vec<_> = (word_size..(8 << 10))
            .map(|size| {
                Layout::from_size_align(size * 128, word_size).unwrap()
            })
            .collect();
        unsafe {
            let mut ptrs = Vec::new();
            for l in &layouts {
                let p = alloc.alloc(l.clone()).expect("alloc should not fail") as *mut usize;
                ptr::write_volatile(p, !0);
                ptrs.push(p);
            }
            for (ptr, l) in ptrs.into_iter().zip(layouts.into_iter()) {
                alloc.dealloc(ptr as *mut u8, l);
            }
        }
    }


    fn multi_threaded_alloc_test(layouts: Vec<Layout>) {
        const N_THREADS: usize = 64;
        let alloc = ElfMallocBuilder::default().build::<MmapSource>();

        let mut threads = Vec::new();
        for _ in 0..N_THREADS {
            let mut my_alloc = alloc.clone();
            let my_layouts = layouts.clone();
            threads.push(thread::spawn(move || unsafe {
                for _ in 0..2 {
                    let mut ptrs = Vec::new();
                    for l in &my_layouts {
                        let p = my_alloc.alloc(l.clone()).expect("alloc should not fail") as *mut usize;
                        ptr::write_volatile(p, !0);
                        ptrs.push(p);
                    }
                    for (ptr, l) in ptrs.into_iter().zip(my_layouts.iter()) {
                        my_alloc.dealloc(ptr as *mut u8, (*l).clone());
                    }
                }
            }))
        }
        for t in threads.into_iter() {
            t.join().expect("threads should not fail")
        }
    }

    #[test]
    fn many_threads_many_sizes() {
        let word_size = mem::size_of::<usize>();
        multi_threaded_alloc_test(
            (word_size..(2 << 10))
                .map(|size| {
                    Layout::from_size_align(size * 1024, word_size).unwrap()
                })
                .collect(),
        );
    }

    #[test]
    fn large_ws_small_size() {
        multi_threaded_alloc_test(
            (1..(8 << 10))
                .map(|_| Layout::from_size_align(64, 64).unwrap())
                .collect(),
        );
    }

    #[test]
    fn large_ws_large_size() {
        multi_threaded_alloc_test(
            (1..(1 << 8))
                .map(|_| Layout::from_size_align(512 << 10, 64).unwrap())
                .collect(),
        );
    }
}
