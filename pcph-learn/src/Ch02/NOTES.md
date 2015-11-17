## Notes for Chapter 2

### Weak Head Normal Form

* Things in WHNF exposes at least it's outermost constructor
    - e.g. for `Maybe` it's `Nothing` or `Just`
    - can still contain thunks (e.g. `Just _`)
    - in contrast, there is Normal Form (NF), in which all fields has to be fully evaluated
        + WHNF but not NF: `Just _`, `[_,_,_]`, `1 :: 2 :: _`, `_ :: _`
        + not WHNF: `_`

* GHCi command `:sprint` can be used to examine the current known
structure of a Haskell expression (noninvasively)

* `seq a b` forces `a` to be evaluated before returning `b`, thus at
the time when we need `b`, we know `a` must have been evaluated (to at least WHNF)

* Sometimes `force :: NFData a => a -> a` is useful for reducing data to normal form,
consider `rpar <computation>` will only try to reduce to WHNF, if the computation produces
a list, it's possible that only first element, or only the `:` constructor is evaluated,
so `rpar (force <computation>)` guarantees us to traverse the result of computation
so everything has to be done completely.

### `rpar` and `rseq`

* From what I can understand, `rpar` to `rseq` is like a non-blocking operation
to its blocking counterpart.

* Both `rpar` and `rseq` put computation into some parallelism framework

* `rpar` does not block rest of the operations (in do-notation)

* `rseq` blocks until the result is available, just like `seq` requires
  its first argument to be WHNF before returning its second argument

### Compilation

Not sure (probably not) whether `runghc` would work, but we'd better
compile the program using arguments `-O2 -threaded -rtsopts` to turn on multithreading support.
We also need RTS option: `-N[x]` (see [here](https://downloads.haskell.org/~ghc/7.0.3/docs/html/users_guide/using-smp.html#parallel-options) for more helps).

### Debugging

* `+RTS -s -RTS` let the runtime system emit the statistics.
