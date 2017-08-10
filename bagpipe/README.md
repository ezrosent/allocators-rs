# Bagpipe

A `Bagpipe` is an implementation of a concurrent pool data-structure. It
is built on top of an implementation of a concurrent queue or stack
data-structure.  It shards requests for pushing and popping objects
among a small set of queues or stacks, with load balancing performed in
a manner that is low in coordination among threads. Contention is automatically
managed by unrolling tight loops within the underlying queues and allowing
threads to attempt pushing on another queue if a CAS failure occurs.

## Other Features

This repo also includes implementations of concurrent queues such as
the `FAAArrayQueue` and the `YangCrummeyQueue`. These are linearizable
non-blocking multi-producer multi-consumer queues that may be of
independent interest, as they scale better than the queues present in
the `crossbeam` library (to my knowledge).
