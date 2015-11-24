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
