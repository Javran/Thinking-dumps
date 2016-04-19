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

## How STM works

* an STM transaction works by storing `writeTVar` in a log rather than applying them
  to memory immediately. To abort some side effects, we simply drop the log.

* when a transaction reaches its end, we check if current content of the memory matches
  that of values read by `TVar`. and if all checks are passed,
  the effect is commited to the memory.
  (I guess it's safe to assume that if no value read by `TVar` is changed,
  then the result is desired. because `TVar` is the only thing you can change inside STM)

Notes:

* `readTVar` is a bit expensive because one needs to traverse the log to see if there are
  any conflicts.

* avoid using an unbounded number of `TVar`s, because `readTVar` will then be slow

* long transactions are more likely to fail and be retried.
  because short transactions will spend less time to run and is faster to commit,
  which forces the long one to retry.
