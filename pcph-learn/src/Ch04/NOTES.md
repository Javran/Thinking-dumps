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

A pipeline consists of 3 roles: producer, mapper and consumer.

* `producer`: a stream source that produces values, it often creates `IVar`
  and forks dedicated task for value-producing, and returns the `IVar` reference.

* `consumer`: consumes value produced from `IVar`.

* `mapper`: mappers do stream transformations, it is both a producer and a consumer:
  it takes values from `IVar`, applies transformation to it and then forks threads for producing
  some other values to be consumed by next worker on the pipeline.

#### Rate-Limiting the Producer

If a mapper processes values at a rate lower than the producer,
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

At beginning I can't read the type. What I can understand is that:

* `Fork` alternative embeds another `IList a`
* We should call the field with type `Par ()` for the effects, here this is most likely
  the computation that restarts value-producing

To actually implement this (see `StreamLimit.hs` for the implementation),
we should take a closer look at the type of `loop` function:
`[a] -> IVar (IList a) -> Par ()` (the order of the argument does not matter).

Thanks to [the hints on Stack Overflow](http://stackoverflow.com/a/24780851/315302),
I can finally figure this out: the `Fork` alternative is used like a tag to `Cons`
to indicate that there are more data whereas the value-producing process is not yet
started. So when a consumer meets this tag, it can fork the embeded computation
to resume value-producing process.

### Limitation of Pipeline Parallelism

The idea of pipeline parallelism is to fork a decicated computation
to process one stage in the pipeline. Obviously this usually can't be as
efficient as data parallelism, but pipeline parallelism is still a power tool.

### Example: A Conference Timetable

* Many problems can be solved by a divide-and-conquer strategy: split
a problem recursively into smaller "subproblems", have them solved separately
then we merge these results together to form the final answer for the original problem.

* **parallel skeleton** I think the basic idea is: the divide-and-conquer strategy has some stages,
if adding parallelism has nothing to do with the actually implementation of how to do so (for exammpl, how a subproblem solution could be turned into a global solution, or how we split
a problem into many subproblems), we can leave these specific operations as arguments.
By doing so we will have a "skeleton", which should have most of the detail abstracted so
we can focus on adding parallelism to the program.

* Parallelism has its own overhead, if tasks are too fine-grained, this overhead
will negate the benefit of having parallelism. In this specific problem,
we set a max-depth, if we want to search any further past this limit, we'd better
just use one worker instead of splitting that task further
