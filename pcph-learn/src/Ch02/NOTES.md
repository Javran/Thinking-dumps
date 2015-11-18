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

### CPU Time and Elapsed Time

Elapsed time is the wall-clock time, which is always used when calculating
speedups. CPU time indicates how busy our cores are, this is usually greater than
elapsed time when multiple cores are running.

### Eventlog and ThreadScope

You'll need following flags for compilation:

```shell
ghc -O2 -threaded -rtsopts -fforce-recomp -eventlog <.hs file>
```

`-fforce-recomp` is there for saving you from removing compiled binaries over and over again.

And following flags should be passed to the RTS:

```shell
./yourprogram +RTS -N -l -s -RTS
```

Only `-l` is required, but it's not bad adding other flags to make the output more informative.

### Static Partitioning vs. Dynamic Partitioning

* Static partitioning: partitioning tasks before the program runs,
so that the work division is fixed

* Dynamic partitioning: distributing smaller units of work among processors at runtime

It's just nature that the workload of each task is uneven, so an important principle is:
``Try to avoid partitioning the work into small, fixed number of chunks''. Dynamic partitioning
make the schedule of doing tasks more flexible. At runtime, different tasks take different among
of resources and time, which is usually hard to predict beforehand. If we can schedule tasks
according to current number of available workers, we are more likely to keep most of the workers
busy so that we gain more benefits of parallelism.

* **spark**: the argument to `rpar`, I think it's like a task to be distributed to workers

* work stealing: see wikipedia for detail. From what I understand, if one worker is available,
it checks other busy workers' task queue and steal tasks from it.

* Amdahl's law: see wikipedia

## DeepSeq

* Traverses the whole data structure and makes sure everything is in its normal form.
Should avoid repeated uses of `deepseq` for this reason.

* To create instances of NFData, one should chain `rnf`-`seq` pattern to cover all
possible fields.

* `Control.Seq` provides some facilities for evaluating strategies between WHNF and NF,
so we have some controls over how deep or how much should we evaluate a data structure.
