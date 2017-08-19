# Bagpipes: Scalable Concurrent Bags

Bagpipes are a simple data-structure that relies on common structure present in
non-blocking queues to provide a more scalable queue-like data-structure with
weaker ordering guarantees. We use them in the `elfmalloc` allocator to act as
repositories for slabs of memory. The scalability of this data-structure allows
more memory to be stored in them, as opposed to in thread-local caches. Our
approach here bares some resemblance to [`scalloc`'s](https://arxiv.org/pdf/1503.09006.pdf)
*span pool* data-structure, itself inspired by the [Distributed Queues](https://www.semanticscholar.org/paper/Distributed-queues-in-shared-memory-multicore-perf-Haas-Lippautz/3c1e0e9c5b774f8d1b7522e7b7ea90634b1e252a)
of Haas et al.

The primary difference between our approach and theirs is:

* We leverage more scalable fetch-add based queues as the basis of the
  data-structure.

* We take a different approach to load-balancing that is lower-overhead at the
  cost of FIFO-like consistency.

## Concurrent queues with transient failures
The basis of a Bagpipe is some non-blocking data-structure with `push` and `pop`
methods. While either stacks or queues work for our purposes, most of the time
we instantiate Bagpipes with queues as they currently perform better in
practice. The only other structure that we rely on is some notion of *transient
failure* that can be attributed to contention among different threads
interacting with the data-structure. One common example is a CAS failure in a
non-blocking queue, or a failed call to `try_lock` in a blocking setting. This
structure is encoded in the `SharedWeakBag` trait. 

```rust
pub enum PopStatus {
    Empty,
    TransientFailure,
}
pub type PopResult<T> = Result<T, PopStatus>;

pub trait SharedWeakBag {
    type Item;
    fn new() -> Self;
    fn try_push(&self, it: Self::Item) -> Result<(), Self::Item>;
    fn try_pop(&self) -> PopResult<Self::Item>;
    fn push(&self, it: Self::Item) {
        let mut cur_item = it;
        while let Err(old_item) = self.try_push(cur_item) {
            cur_item = old_item
        }
    }
    fn pop(&self) -> Option<Self::Item> {
        loop {
            return match self.try_pop() {
                Ok(it) => Some(it),
                Err(PopStatus::Empty) => None,
                Err(PopStatus::TransientFailure) => continue,
            };
        }
    }
}
```

### Recent queues of interest

Over the past few years, a number of queue designs have been proposed that
improve upon the standard non-blocking queue design of [Michael and
Scott](https://www.research.ibm.com/people/m/michael/podc-1996.pdf). Instead of
designs where enqueuers and dequeuers contend on a single head or tail node in a
CAS loop, these algorithms instead use the atomic `fetch-add` instruction to
acquire a cell in the array to enqueue or dequeue a particular node. To our
knowledge, the first queue with this basic design was [Morrison and Afek
(2013)](http://www.cs.tau.ac.il/~mad/publications/ppopp2013-x86queues.pdf).
While the queue in this paper outperforms the ones we currently use, it relies
on a double-word CAS instruction. This makes the queue both non-portable, and
hard to implement in Rust, as that primitive is not exposed in the `atomic`
package at the moment.

Still, there are two queues that perform almost as well that do not rely on this
instruction:

* An implementation of the obstruction-free algorithm alluded to in Morrison and
  Afek's original paper, based on the "fast path" in [Yang-Muller and Crummy
  (2016)](http://chaoran.me/assets/pdf/wfq-ppopp16.pdf).

* An implementation of Concurrency Freaks' [Fetch-add Array Queue](http://concurrencyfreaks.blogspot.com/2016/11/faaarrayqueue-mpmc-lock-free-queue-part.html)

Both of these queues can exhibit transient failures if a receiver reaches a cell
before a matching sender.

## Throwing away consistency

Following work like [`scalloc` (Aigner et al)](https://arxiv.org/abs/1503.09006),
we store reusable slabs of objects in global data-structures in order to
minimize unused memory in thread-local caches. We aim to decrease the size of
caches even further than `scalloc` does (indeed, it is possible to configure
`elfmalloc` with no cache whatsoever beyond a single slab). The queues listed
above are quite scalable, but they do eventually reach bottlenecks due to the
limitations of the fetch-add primitive in use. To increase scalability, we
simply throw away the consistency guarantees (in the above case,
linearizability) of the queue in question.

It is normally a dubious move to throw away linearizability. Indeed, there is a
reasonable case to be made that a concurrent queue that is not linearizable is
not really a correct (FIFO) queue at all. However, we do not not require FIFO
semantics from our queues, we simply require that they do not lose their
contents. 

## Revocation

One extra operation that we require in `elfmalloc` is the ability to remove a
node that is somewhere in the queue. To do this, certain `Revocable` types hold
handles that can store a pointer into the queue cell in which they are pushed.
Once in place, a very fast `revoke` operation can be performed to CAS this cell
to a sentinel value:

```rust
pub trait Revocable { fn handle(&self) -> &AtomicUsize; }
pub trait RevocableWeakBag: SharedWeakBag
    where Self::Item: Revocable
{
    /// Attempt to remove `it` from the bag, returning `true` if successful.
    ///
    /// This operation is unsafe because the underlying implementation may assume that `it` is (or
    /// was) successfully pushed into the bag at some point.
    unsafe fn revoke(it: &Self::Item) -> bool;
}
```

### Bagpipes

A `Bagpipe` is simply a data-structure that holds an array of `SharedWeakBag`s.
Threads interacting with a `Bagpipe` traverse random permutations of this array,
calling `try_pop` and `try_push` on each individual "pipe" they encounter. The
semantics of the *try* methods from the `SharedWeakBag` trait are such that
threads only stop pushing to or popping from queues that are under load. This
allows for some light-weight load-balancing of the queues without any additional
coordination required.

This structure also allows threads to bail out early if they are preempted too
often. In this case, the thread can use some fallback mechanism in the allocator.

## Performance

The queues that a `Bagpipe` wraps are both structured as linked lists of arrays
of cells. These arrays are referred to as *segments*. The precise performance of
different `Bagpipes` is highly sensitive to the size of these segments. On our
current test machine with 32 threads, enqueue-dequeue pairs for a `Bagpipe`
backed by a fetch-add array queue had throughput of 100Mops/second. By contrast,
a Trieber stack had 7Mops/second and a single array queue had a throughput of
25Mops/s. `Bagpipe`s do not perform quite as well in a producer-consumer
setting, but they still outperform any other queue by over 2x.

Better performance of these data-structures is possible at the cost of increased
memory usage. While they are currently configured for the more memory-cautious
environment of an allocator, we plan to make them configurable to optimize for
throughput, either via feature flags or type parameters. Anecdotally, I believe
I got the Yang Crummey-backed `Bagpipe` to get 200Mops/s, but that required
64KiB segments, increasing steady-state memory consumption by an order of
magnitude.

The benchmark code is in the `bagpipe/src/bin/bench_bag.rs` directory.
