<!-- Copyright 2017 the authors. See the 'Copyright and license' section of the README.md file at the top-level directory of this repository.

Licensed under the Apache License, Version 2.0 (the LICENSE file). This file may not be copied, modified, or distributed except according to those terms. -->

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
