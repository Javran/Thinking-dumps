# Notes on Chapter 10

* `TVar` is a mutable variable inside `STM` monad, which is atomic when executing.

* because `STM` computations are atomic, they can use unlimited number of `TVar`s and don't need
  to worry about the ordering of acquiring / releasing.

* `atomically :: STM a -> IO a` executes a `STM` computation in `IO` monad to yield the
  final result.

* One notable feature of `STM` is that atomic operations can be composed
  and the resulting computation is still atomic.

* `STM` monad has the ability to roll back the effects of a transaction.
  But the tradeoff is that we cannot perform arbitrary computations which
  might has side effects that the `STM` monad doesn't know how to rollback.
