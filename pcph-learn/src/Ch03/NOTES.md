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
