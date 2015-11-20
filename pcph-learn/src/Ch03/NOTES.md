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
