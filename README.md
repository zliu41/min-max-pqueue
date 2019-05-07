# min-max-pqueue

A min-max priority queue provides efficient access to both its least element
and its greatest element. Also known as
[double-ended priority queue](https://en.wikipedia.org/wiki/Double-ended_priority_queue).

This library provides two variants of min-max priority queues:

- [`MinMaxQueue prio a`](https://hackage.haskell.org/package/min-max-pqueue/docs/Data-MinMaxQueue.html), a general-purpose min-max priority queue.
- [`IntMinMaxQueue a`](https://hackage.haskell.org/package/min-max-pqueue/docs/Data-IntMinMaxQueue.html), a min-max priority queue where priority values are integers.

A min-max priority queue can be configured with a maximum size. Each time an insertion
causes the queue to grow beyond the size limit, the greatest element
will be automatically removed (rather than rejecting the insertion).

Their implementations are backed by `Map prio (NonEmpty a)` and
`IntMap (NonEmpty a)`, respectively. This means
that certain operations are asymptotically more expensive than
implementations [backed by mutable arrays](https://dl.acm.org/citation.cfm?id=6621),
e.g., `peekMin` and `peekMax` is *O(n* log *n)* vs. *O(n)*, `fromList` is
also *O(n* log *n)* vs. *O(n)*. In a pure language like Haskell, a
mutable array based implementation would be impure
and need to operate inside monads. And in many applications, regardless
of language, the additional time complexity would be a small or negligible
price to pay to avoid destructive updates anyway.

If you only access one end of the queue (i.e., you need a regular
priority queue), an implementation based on a kind of heap that is more
amenable to purely functional implementations, such as binomial heap
and pairing heap, is *potentially* more efficient. But always benchmark
if performance is important; in my experience `Map` *always* wins, even for
regular priority queues.

## Advantages over Using Maps Directly

- `size` is *O(1)*, vs. *O(n)* for maps. Note that `Data.Map.size` is *O(1)* but it
  returns the number of keys, which is not the same as the number of elements in
  the queue. `Data.IntMap.size`, on the other hand, is *O(k)* where *k* is
  the number of keys.
- A queue can have a size limit, and it is guaranteed that its size
  does not grow beyond the limit.
- Many useful operations, such as `takeMin`, `dropMin`, are non-trivial to
  implement with `Map prio (NonEmpty a)` and `IntMap (NonEmpty a)`.
- The queue's fold operations operate on individual elements, as opposed to
  `NonEmpty a`.


## Alternative Implementation

In Haskell, an alternative to the mutable array based implementation is
to use immutable, general purpose arrays such as `Seq`. This would achieve
*O(1)* `peekMin` and `peekMax`, but since `lookup` and `update` for `Seq`
cost *O(n* log  *n)*, the cost of `insert`, `deleteMin` and `deleteMax` would
become *O(n* log<sup>2</sup> *n)*.

[A `Seq`-based implementation](https://github.com/zliu41/min-max-pqueue/blob/master/benchmark/SeqQueue.hs) is provided for benchmarking purposes, which,
as shown below, is more than an order of magnitude slower than the `Map`-based implementation
for enqueuing and dequeuing 200,000 elements, proving that the improved
time complexity of `peekMin` and `peekMax` is not worth the cost. In fact,
if you perform `peekMin` and `peekMax` much more often than enqueuing and
dequeuing operations, which means you perform `peekMin` and `peekMax` many times
on the same queue, you should simply memoize the results.

## Benchmarks

Benchmarking was done on my laptop in which 200,000 elements (which are
integers) are inserted into the queue and subsequently removed one after
another.

- `pq`, `intpq` and `sq` represents `MinMaxQueue`, `IntMinMaxQueue` and
  `SeqQueue`.
- `asc`, `desc` and `rand` represents inserting the elements in ascending,
  descending and random order.
- `min` and `max` represents removing elements from the min-end and max-end.

As seen in the following result, `IntMinMaxQueue` is twice as fast as
`MinMaxQueue` for integer keys, whereas `SeqQueue` is more than an order
of magnitude slower.

```
benchmarking intpq-asc-min          
time                 27.15 ms   (23.85 ms .. 29.31 ms)
                     0.972 R²   (0.927 R² .. 0.997 R²)
mean                 30.84 ms   (29.67 ms .. 35.07 ms)
std dev              4.308 ms   (1.160 ms .. 7.915 ms)
variance introduced by outliers: 57% (severely inflated)

benchmarking intpq-desc-max         
time                 29.70 ms   (29.23 ms .. 30.41 ms)
                     0.998 R²   (0.995 R² .. 1.000 R²)
mean                 30.62 ms   (30.29 ms .. 31.02 ms)
std dev              803.7 μs   (548.0 μs .. 1.190 ms)

benchmarking intpq-rand-min         
time                 31.00 ms   (29.10 ms .. 33.05 ms)
                     0.985 R²   (0.973 R² .. 0.994 R²)
mean                 28.39 ms   (27.43 ms .. 29.46 ms)
std dev              2.216 ms   (1.968 ms .. 2.591 ms)
variance introduced by outliers: 32% (moderately inflated)

benchmarking intpq-rand-max         
time                 30.96 ms   (28.98 ms .. 32.96 ms)
                     0.987 R²   (0.976 R² .. 0.996 R²)
mean                 33.66 ms   (32.71 ms .. 34.49 ms)
std dev              1.820 ms   (1.473 ms .. 2.388 ms)
variance introduced by outliers: 18% (moderately inflated)

benchmarking pq-asc-min             
time                 69.02 ms   (61.94 ms .. 72.95 ms)
                     0.988 R²   (0.968 R² .. 0.997 R²)
mean                 71.41 ms   (68.99 ms .. 74.35 ms)
std dev              4.799 ms   (3.401 ms .. 6.974 ms)
variance introduced by outliers: 17% (moderately inflated)

benchmarking pq-desc-max            
time                 80.90 ms   (78.68 ms .. 85.06 ms)
                     0.997 R²   (0.994 R² .. 0.999 R²)
mean                 83.20 ms   (80.91 ms .. 89.15 ms)
std dev              5.853 ms   (2.234 ms .. 9.957 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking pq-rand-min            
time                 65.80 ms   (60.01 ms .. 69.62 ms)
                     0.987 R²   (0.965 R² .. 0.996 R²)
mean                 74.17 ms   (70.93 ms .. 79.86 ms)
std dev              7.495 ms   (4.557 ms .. 12.39 ms)
variance introduced by outliers: 35% (moderately inflated)

benchmarking pq-rand-max            
time                 68.29 ms   (65.07 ms .. 70.84 ms)
                     0.997 R²   (0.995 R² .. 1.000 R²)
mean                 74.03 ms   (71.64 ms .. 77.51 ms)
std dev              5.016 ms   (3.110 ms .. 7.556 ms)
variance introduced by outliers: 17% (moderately inflated)

benchmarking sq-asc-min             
time                 1.954 s    (1.369 s .. 2.838 s)
                     0.971 R²   (NaN R² .. 1.000 R²)
mean                 1.733 s    (1.592 s .. 1.861 s)
std dev              160.1 ms   (28.78 ms .. 203.2 ms)
variance introduced by outliers: 22% (moderately inflated)

benchmarking sq-rand-min            
time                 2.889 s    (2.000 s .. 3.658 s)
                     0.989 R²   (0.959 R² .. 1.000 R²)
mean                 2.915 s    (2.828 s .. 3.054 s)
std dev              130.7 ms   (442.1 μs .. 159.9 ms)
variance introduced by outliers: 19% (moderately inflated)
```
