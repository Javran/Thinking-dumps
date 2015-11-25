## Notes for Chapter 4

### Par monad

* More explicit about granularity and data dependencies, avoid relying on laziness
* Computation can communicate through `IVar`.
* 3 primitives for manipulating `IVar`: `new`, `put`, `get`
    * `new`: creates a new `IVar`, conceptually think it as a box that
      might or might not contain a value
    * `get`: retrieve value from an `IVar`, waits until it contains a result
    * `put`: put value into `IVar`, note that it requires the contained value to be `NFData`.
      This is to make sure the result don't have any unevaluated parts.
      Also note that when a value is put in the box, `get` won't remove it and
      the value stays there. So multiple `put` calls on same `IVar` are considered error.
      (Recall that we should avoid deepseq'ing on same value as it tries to traverse
      the whole data structure, the library provides us a backdoor function `put_`
      that only evaluates value to WHNF. If we know something is already in NF,
      we can also use it to shortcut the computation)

### Example: Shortest Path in a Graph

This section targets at parallelizing Floyd-Warshall algorithm.

* folds vs. maps
    * folds can be parallelized only when the operation is associative (e.g. sum or product).
      If this is the case, the linear fold can be turned into a tree, which can in turn be
      viewed as data dependency graph.

    * maps can be parallelized nicely. (no data dependency between elements)
* traverse: some data structure can be traversed, effects can then be accumulated.
  `IntMap` is one of them that has `traverseWithKey` to do so. (If an data structure
  has supports for traversal, we'd better use it instread of creating an explicit list
  and traverse the list)

### Pipeline Parallelism

A different way to expose parallelism (play nicely with data streams).
In lazy streams we often want to apply transformation on streams which forms a pipeline.
The idea is to assign each core with one stream transformation.
Here each core plays role of producer or consumer and work together to produce the final result.

In book we have a data structure representing a lazy list:

```haskell
data IList a
  = Nil
  | Cons a (IVar (IList a))

type Stream a = IVar (IList a)
```

By wrapping the tail in an `IVar`, we can allow other threads
to compute it.

A pipeline consists of 3 roles: producer, transformer and consumer.

* `producer`: a stream source that produces values, it often creates `IVar`
  and forks dedicated task for value-producing, and returns the `IVar` reference.

* `consumer`: consumes value produced from `IVar`.

* `mapper`: mappers do stream transformations, it is both a producer and a consumer:
  it takes values from `IVar`, applies transformation to it and then forks threads for producing
  some other values to be consumed by next worker on the pipeline.

#### Rate-Limiting the Producer

If a transformer processes values at a rate lower than the producer,
many unconsumed values will be accumulated, this is undesired, the trick
is to just produce some limited results and pause,
if it turns out later that more values are required,
we can resume the value-producing process.

In book, we make the following extension on `IList` to allow this:

```haskell
data IList a
  = Nil
  | Cons a (IVar (IList a))
  | Fork (Par ()) (IList a)
```

(TODO: this is not yet verified) I think there is another way of extending `IList`:

```haskell
data IList a
  = Nil
  | Cons a (IVar (IList a))
  | Fork (Par (IList a))
```
