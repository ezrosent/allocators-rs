// TODO:
// - When different cores are accessing this, do we have false cache line sharing issues?
// - Currently, when we're resizing, every time we move a pointer from the current bucket to its
//   new bucket, we just call insert() on the new bucket. This re-traverses the bucket from the
//   beginning. We can do better - we could keep track of what bucket/index we're currently at, and
//   start the next search from that point.
// - Once Vec and Box support parametric allocators, use that functionality.

// Design
//
// This hash table is designed to perform well under the conditions present when used with the
// large slab allocator. In particular, it is designed for workloads where:
// - Insertions and deletions are infrequent, but lookups are very common (lookups are caused by
//   allocs or deallocs, while insertions and deletions are caused by allocing or freeing entire
//   slabs)
// - An already-inserted value will never be inserted again, and a non-existent value will never be
//   deleted
// - Keys are pointers which all share a common alignment
// - Since, for the most common page size (4096 bytes), the smallest object for which large slabs
//   are used is ~512 bytes, the space used by the hash table is assumed to be dwarfed by the space
//   used to back allocated objects. Thus, space is taken to be cheap.
// - The slab allocator's working set algorithm should prevent thrashing. That is, even if a large
//   number of objects are freed very quickly and then allocated very quickly, the working set
//   algorithm will refuse to free these slabs. The only way for thrashing to be a problem is for
//   the sequence of frees and the sequence of allocs to be separated by a full working period
//   (15 seconds as of the writing of this comment).
//
// Given this workload profile, we make the following design decisions:
// - We assume that, since insertions and deletions are infrequent, the cost of allocating and
//   freeing buckets is negligible. However, the cost of traversing pointers in a bucket chain is
//   NOT negligible. Thus:
//   - We make sure to always keep buckets coalesced (with all full slots bunched at the beginning
//     before any empty slots) at the cost of a slightly more expensive delete operation.
// - The hash table does not support inserting existing keys or deleting non-existent keys. This
//   makes the implementation somewhat simpler.
// - Since keys are pointers with a common alignment, the hash function is trivial: shift the key
//   by the alignment. This produces some patterns in what buckets are used, but in practice most
//   buckets end up with 2-3 elements and occasionally 4 elements (note that this is dependent on
//   our choice of when to resize, which is discussed below). Note, however, that the shift is
//   very important! Without it, keys would all cluster in the lower 1/nth of the table (where n
//   is the alignment).
// - Since space is cheap, we choose buckets to have 4 slots and for the hash table to grow (by
//   doubling the number of buckets) as soon as the number of elements stored is equal to the
//   number of buckets. In practice, we've observed that this results in buckets almost never
//   overflowing. This choice also means that the hash table takes up at most approximately
//   512/(4 * 8) = 1/16 of the space required by the allocated objects themselves.
// - Since high-frequency thrashing is prevented by the slab allocator's working set algorithm, it
//   is safe to not only use the growing scheme described in the previous bullet, but also to use
//   a similar shrinking algorithm - to halve the number of buckets when the number of elements
//   stored is equal to half the number of buckets.

extern crate alloc;
use core::ptr;

const BUCKET_SIZE: usize = 4;

pub struct PtrHashMap<K, V> {
    vec: Vec<Bucket<K, V>>,
    size: usize, // number of elements stored in the map
    align_shift: u32,
}

impl<K, V> PtrHashMap<K, V> {
    /// Constructs a new `PtrHashMap`.
    ///
    /// `size` is a hint for how many elements will be stored in the map.
    ///
    /// `align` should be a liberal estimate of the alignment shared by all keys that will be
    /// stored in the table. For example, if some pointers will have at least alignment 4 and
    /// others will have at least alignment 8, set `align = 8`.
    pub fn new(size: usize, align: usize) -> Self {
        use core::mem;
        assert!(align >= mem::align_of::<K>());

        // size must be power of two
        let size = (size as u64).next_power_of_two() as usize;
        let mut vec = Vec::with_capacity(size);
        for _ in 0..size {
            vec.push(Bucket::default());
        }
        PtrHashMap {
            vec,
            size: 0,
            align_shift: align.trailing_zeros(),
        }
    }

    /// Dumps all of the buckets in the table.
    ///
    /// `dump_by_bucket` creates a copy of the table, with each bucket represented by a `Vec<(*mut
    /// K, *mut V)>`.
    #[cfg_attr(not(test), allow(unused))]
    pub fn dump_by_bucket(&self) -> Vec<Vec<(*mut K, *mut V)>> {
        let mut res = Vec::with_capacity(self.vec.len());
        for b in &self.vec {
            res.push(b.dump());
        }
        res
    }

    #[inline]
    fn hash(&self, ptr: *mut K) -> usize {
        self.hash_with_len(ptr, self.vec.len())
    }

    // len must be a power of two
    #[inline]
    fn hash_with_len(&self, ptr: *mut K, len: usize) -> usize {
        // NOTE: Since len is a power of two, x & (len - 1) is equivalent to x % len
        ((ptr as usize) >> self.align_shift) & (len - 1)
    }

    #[inline]
    fn get_bucket(&self, ptr: *mut K) -> &Bucket<K, V> {
        unsafe { self.vec.get_unchecked(self.hash(ptr)) }
    }

    #[inline]
    fn get_bucket_mut(&mut self, ptr: *mut K) -> &mut Bucket<K, V> {
        unsafe {
            let h = self.hash(ptr);
            self.vec.get_unchecked_mut(h)
        }
    }

    // Get the value associated with the given key. The key must exist, or else get will panic.
    #[inline]
    pub fn get(&self, ptr: *mut K) -> *mut V {
        self.get_bucket(ptr).get(ptr)
    }

    /// Inserts a new key/value pair.
    ///
    /// # Panics
    /// The key must not already exist in the table, or else `insert` will panic.
    #[inline]
    pub fn insert(&mut self, ptr: *mut K, v: *mut V) {
        debug_assert!(!ptr.is_null());
        #[cfg(not(feature = "hashmap-no-resize"))]
        {
            if self.size == self.vec.len() {
                self.grow();
            }
        }

        let h = self.hash(ptr);
        let bkt = unsafe { self.vec.get_unchecked_mut(h) };
        bkt.insert(ptr, v);
        self.size += 1;
    }

    /// Deletes a key/value pair from the table.
    ///
    /// # Panics
    /// The key must already exist in the table, or else `delete` will panic.
    #[inline]
    pub fn delete(&mut self, ptr: *mut K) {
        debug_assert!(!ptr.is_null());
        #[cfg(not(feature = "hashmap-no-resize"))]
        {
            if self.size == self.vec.len() / 2 {
                self.shrink();
            }
        }
        self.get_bucket_mut(ptr).delete(ptr);
        self.size -= 1;
    }

    fn grow(&mut self) {
        let old_len = self.vec.len();
        let new_len = self.vec.len() * 2;
        debug_assert!(new_len.is_power_of_two());
        self.vec.resize(new_len, Bucket::default());
        debug_assert_eq!(self.vec.len(), new_len);

        for i in 0..old_len {
            // While we're looking for values to move, we're also using this as an
            // opportunity to coalesce values by packing them into the front of the bucket.
            // This both addresses the holes we've made by moving values and also any holes
            // left by previous delete operations. NOTE: Since most traffic on this hash table is
            // lookup traffic, this optimization is REALLY important for performance. Be very
            // careful about changing the coalescing strategy.
            //
            // We do this by keeping track of the first empty slot we've encountered, and
            // when we find a pointer that we're not going to move to another bucket, we
            // see whether it can be moved closer to the front by being placed in that
            // empty slot. When we do this, we re-scan starting at the slot we just filled
            // to find the next empty slot.

            // first_free_bkt.is_null() is a sentinal value indicating that these variables are not
            // set.
            let (mut first_free_bkt, mut first_free_idx) = (ptr::null_mut() as *mut Bucket<K, V>,
                                                            0);
            macro_rules! update_first_free {
                ($bkt:expr, $idx:expr) => (
                    if first_free_bkt.is_null() {
                        first_free_bkt = $bkt;
                        first_free_idx = $idx;
                    }
                )
            }
            let mut cur_bkt = unsafe { self.vec.get_unchecked_mut(i) as *mut Bucket<K, V> };
            while !cur_bkt.is_null() {
                for j in 0..BUCKET_SIZE {
                    let (ptr, v) = unsafe { (*cur_bkt).data[j] };
                    if ptr.is_null() {
                        update_first_free!(cur_bkt, j);
                        continue;
                    }

                    if ((ptr as usize) >> self.align_shift) & old_len != 0 {
                        unsafe {
                            (*cur_bkt).data[j] = (ptr::null_mut(), ptr::null_mut());
                        }
                        update_first_free!(cur_bkt, j);
                        let new_idx = i + old_len;
                        debug_assert_eq!(self.hash(ptr), new_idx);
                        let new_bkt = unsafe { self.vec.get_unchecked_mut(new_idx) };
                        new_bkt.insert(ptr, v);
                    } else {
                        debug_assert_eq!(self.hash(ptr), i);
                        if !first_free_bkt.is_null() {
                            unsafe {
                                // there's an earlier free slot - move this pointer there
                                (*cur_bkt).data[j] = (ptr::null_mut(), ptr::null_mut());
                                (*first_free_bkt).data[first_free_idx] = (ptr, v);
                                // Now that we've filled this slot, we need to find the
                                // next empty slot. We're guaranteed to only search at most
                                // up to the slot we just moved from since we know it's now
                                // empty.
                                while !(*first_free_bkt).data[first_free_idx].0.is_null() {
                                    if first_free_idx == BUCKET_SIZE - 1 {
                                        use core::ops::DerefMut;
                                        first_free_bkt =
                                            (*first_free_bkt).next.as_mut().unwrap().deref_mut() as
                                            *mut Bucket<K, V>;
                                        first_free_idx = 0;
                                    } else {
                                        first_free_idx += 1;
                                    }
                                }
                            }
                        }
                    }
                }
                cur_bkt = match unsafe { (*cur_bkt).next.as_mut() } {
                    None => ptr::null_mut(),
                    Some(bkt) => {
                        use core::ops::DerefMut;
                        bkt.deref_mut() as *mut Bucket<K, V>
                    }
                };
            }
        }
    }

    fn shrink(&mut self) {
        let old_len = self.vec.len();
        let new_len = self.vec.len() / 2;
        for i in new_len..old_len {
            let mut cur_bkt = unsafe { self.vec.get_unchecked_mut(i) as *mut Bucket<K, V> };
            while !cur_bkt.is_null() {
                for j in 0..BUCKET_SIZE {
                    let (ptr, v) = unsafe { (*cur_bkt).data[j] };
                    if !ptr.is_null() {
                        let new_idx = i - new_len;
                        let new_bkt = unsafe { self.vec.get_unchecked_mut(new_idx) };
                        new_bkt.insert(ptr, v);
                    }
                }

                cur_bkt = match unsafe { (*cur_bkt).next.as_mut() } {
                    None => ptr::null_mut(),
                    Some(bkt) => {
                        use core::ops::DerefMut;
                        bkt.deref_mut() as *mut Bucket<K, V>
                    }
                }
            }
        }
        self.vec.truncate(new_len);
        self.vec.shrink_to_fit();
    }
}

#[inline]
fn new_buckets<K, V>() -> [(*mut K, *mut V); BUCKET_SIZE] {
    [(ptr::null_mut(), ptr::null_mut()); BUCKET_SIZE]
}

pub struct Bucket<K, V> {
    data: [(*mut K, *mut V); BUCKET_SIZE],
    next: Option<Box<Bucket<K, V>>>,
}

impl<K, V> Clone for Bucket<K, V> {
    fn clone(&self) -> Bucket<K, V> {
        Bucket {
            data: self.data,
            next: None,
        }
    }
}

impl<K, V> Default for Bucket<K, V> {
    fn default() -> Bucket<K, V> {
        Bucket {
            data: new_buckets(),
            next: None,
        }
    }
}

impl<K, V> Bucket<K, V> {
    #[cfg_attr(not(test), allow(unused))]
    fn dump(&self) -> Vec<(*mut K, *mut V)> {
        let mut res = Vec::new();
        for i in &self.data {
            if !i.0.is_null() {
                res.push(*i);
            }
        }
        if let Some(ref next) = self.next {
            res.extend(next.dump());
        }
        res
    }

    #[inline]
    fn get(&self, ptr: *mut K) -> *mut V {
        for i in &self.data {
            if ptr == i.0 {
                return i.1;
            }
        }
        self.next.as_ref().unwrap().get(ptr)
    }

    fn delete(&mut self, ptr: *mut K) {
        for i in 0..BUCKET_SIZE {
            unsafe {
                if self.data.get_unchecked(i).0 == ptr {
                    #[cfg(not(feature = "hashmap-no-coalesce"))]
                    {
                        let new = self.remove_last(i + 1);
                        *self.data.get_unchecked_mut(i) = new.0;
                    }
                    #[cfg(feature = "hashmap-no-coalesce")]
                    {
                        *self.data.get_unchecked_mut(i) = (ptr::null_mut(), ptr::null_mut());
                    }
                    return;
                }
            }
        }
        self.next.as_mut().unwrap().delete(ptr);
    }

    // The coalesce algorithm is simple: It guarantees the invariant that all of the full slots in
    // a chain of buckets are contiguous. It does this by, following a deletion, moving the last
    // element in the list to take its place. This is accomplished by the remove_last method.

    // Starting at the idx index into this bucket, find the last slot in this bucket chain that is
    // non-empty. Zero it out and return it. Also return a boolean indicating whether this bucket
    // is now empty (and should thus be deleted).
    fn remove_last(&mut self, idx: usize) -> ((*mut K, *mut V), bool) {
        let mut last = (ptr::null_mut(), ptr::null_mut());
        let mut last_idx = None;
        for i in BUCKET_SIZE..idx {
            unsafe {
                if !self.data.get_unchecked(i).0.is_null() {
                    last = *self.data.get_unchecked(i);
                    last_idx = Some(i);
                }
            }
        }

        if let Some(last_idx) = last_idx {
            // This bucket isn't empty. Thus, it might have children.
            if let Some(mut next) = self.next.take() {
                // We have children, so the last full slot will be in one of those children.
                let (kv, remove) = next.remove_last(0);
                if !remove {
                    // Calling self.next.take() implicitly set self.next to None, so if we
                    // shouldn't be removing it, then set it back to Some(next) like it was before.
                    self.next = Some(next);
                }
                (kv, false)
            } else {
                // We don't have any children. Thus, this is really the last full slot. Zero it out
                // and return it.
                unsafe {
                    *self.data.get_unchecked_mut(last_idx) = (ptr::null_mut(), ptr::null_mut());
                }
                // If we just deleted the first (and thus only) slot in the bucket, then this
                // bucket is now empty and should be deleted.
                (last, last_idx == 0)
            }
        } else {
            // We didn't find anything. Since fully-empty buckets are guaranteed to never exist,
            // this means that idx must have been larger than 0.
            debug_assert!(idx > 0);
            // Since buckets are guaranteed to not have any gaps, this means that we're the last
            // bucket and so our search is done. Since idx is greater than zero, we know that this
            // bucket isn't empty, and thus shouldn't be deleted.
            ((ptr::null_mut(), ptr::null_mut()), false)
        }
    }

    fn insert(&mut self, ptr: *mut K, v: *mut V) {
        for i in 0..BUCKET_SIZE {
            unsafe {
                let cur = self.data.get_unchecked_mut(i);
                debug_assert_ne!(ptr, cur.0);
                if cur.0.is_null() {
                    *cur = (ptr, v);
                    return;
                }
            }
        }

        if let Some(next) = self.next.as_mut() {
            next.insert(ptr, v);
            return;
        }
        let mut data = new_buckets();
        data[0] = (ptr, v);
        self.next = Some(Box::new(Bucket {
                                      data: data,
                                      next: None,
                                  }));
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basic_map_functionality() {
        const SIZE: usize = 1 << 20;
        let elts: Vec<usize> = (0..SIZE).collect();
        let mut hs = PtrHashMap::new(SIZE, 8);
        let raw_iter = || {
            elts.iter()
                .map(|i| (i as *const _ as *mut usize, (i + 1) as *mut usize))
        };
        for i in raw_iter() {
            hs.insert(i.0, i.1);
        }
        for i in raw_iter() {
            assert_eq!(hs.get(i.0), i.1);
        }
        for (ix, i) in raw_iter().enumerate() {
            if ix % 2 == 0 {
                hs.delete(i.0);
            }
        }
        for (ix, i) in raw_iter().enumerate() {
            if ix % 2 != 0 {
                hs.get(i.0);
            }
        }
    }
}
