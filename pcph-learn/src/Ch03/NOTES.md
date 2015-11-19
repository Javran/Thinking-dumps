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
* (NOT CONFIRMED) `rparWith` can be combined with `rdeepseq`, by doing so we
can create sparks to full evaluate some data structure but without waiting for the result
to proceed.
* I guesss `dot :: Strategy a -> Strategy a -> Strategy a` could be an important combinator,
understanding what it does it mean to combine 2 strategies might help us understand `rparWith`
