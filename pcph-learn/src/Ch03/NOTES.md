## Notes for Chapter 3

### Strategy

Quote from book:

> Evaluation Strategies are a means for modularizing parallel code
> by separating the algorithm from the parallelism.

We have previously seen how to use `Eval` monad to make computations
work in parallel. Now we have `Strategies`.
From what I can understand, it encourages us to build up evaluation
strategies with primitives like `rseq` `rpar` and combinators like `evalList`.

```haskell
type Strategy a = a -> Eval a

using :: a -> Strategy a -> a
```

`using` is often written using infix. On the left hand side of it is ``what the program does''
and on the right hand side we have ``how to add parallelism to it''.
It has the nice property that it is usually the case that removing ``(`using` strat) ``
does not change the result of the expression.

### Parametrized Strategies

* Both `rpar` and `rseq` are of type `a -> Eval a` so they can be viewed as strategies.
* `rdeeqseq` is like `rseq` but evaluates its argument to NF (and blocks until done)
* `rparWith` can be combined with `rdeepseq`, by doing so we
can create sparks to full evaluate some data structure but without waiting for the result
to proceed.
* I guesss `dot :: Strategy a -> Strategy a -> Strategy a` could be an important combinator,
understanding what it does it mean to combine 2 strategies might help us understand `rparWith`

#### `rparWith` explanation

I could be wrong but let me try to explain what `rparWith` does in a different way:

It's easy to come with a combinator that works for pairs:

```haskell
evalPair :: Strategy a -> Strategy a -> Strategy a
evalPair sa sb (a,b) = do
    a' <- sa a
    b' <- sb b
    return (a',b')
```

Now we have a `Strategy` that fully evaluates a value:

```haskell
rdeepseq :: NFData a => Strategy a
rdeepseq x = rseq (force x)
```

Now it's easy to figure out how to fully evaluate components of a pair:

```haskell
seqPair :: (NFData a, NFData b) => Strategy (a,b)
seqPair = evalPair rdeepseq rdeepseq
```

Thanks to referential transparency we can expand `seqPair` a little bit:

```haskell
seqPair :: (NFData a, NFData b) => Strategy (a,b)
seqPair (a,b) = do
    a' <- (rseq . force) a
    b' <- (rseq . force) b
    return (a',b')
```

Now we can see a problem here: `rseq (force a)` is blocking `rseq (force b)`,
but we really want both components to be evaluated in parallel.

With `rparWith`, we can ensure that a strategy is put into parallel and not
blocking other tasks:

```haskell
seqPair :: (NFData a, NFData b) => Strategy (a,b)
seqPair (a,b) = do
    a' <- (rparWith (rseq . force)) a
    b' <- (rparWith (rseq . force)) b
    return (a',b')
```

So we can write `parPair` without concerns about blocking:

```haskell
parPair :: Strategy a -> Strategy b -> Strategy (a,b)
parPair sa sb = evalPair (rparWith sa) (rparWith sb)
```

### Evaluating a List in Parallel

Nothing new here: `evalList` is like `evalPair` and `parList` is like `parList`.
By using these two combinators for lists, we are able to compute
elements of the list in parallel.

### K-Means Example

**Let program measure its own time.**  Sometimes we might want the program to display its own running time instead of relying
on an external time-measuring mechanism. By doing so we might get benefit of knowing
more fine-grained running time at the cost of writing few more lines for time measurement.
In our case the I/O cost is not our focus of optimization, by ignoring this part,
the speedup more accurately reflects how good our optimization is.

**Some extra factors to consider**

There are few more factors we need to consider
for putting code into parallelism:

* **How to split tasks and combine results.** It's possible to allow all elements of a list
to be computed in parallel, but too fine-grained tasks might nullify the benefit of parallelism
because of the overhead added. So what we do is to group small tasks into chunks, and process them in parallel. Further, when the computations are done, we also need a way to combine chunks' results. (`parListChunk` might be good for solving this problem)

* **To what extent do we need to split our tasks.** This might be left as a runtime configuration.
Previously we know that static partitioning is not good in general, and I think the same
principle applies here. Depending on the machine on which we run the code, the optimial configuration varies, and for enabling the program to scale we probably need to leave it as a runtime argument.

* **Be cautious of foreign calls.** A foreign call usually indicates some kind of I/O, if
we put some unnecessary I/O works (like debugging outputs between iterations), we'll
get speedup penalty, sometimes this is big.

* **Extra costs about chunks** Creating chunks takes time and combining results also do,
keep in mind that the process of combining results of chunks might be sequential.
