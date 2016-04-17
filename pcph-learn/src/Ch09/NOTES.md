# Notes on Chapter 9

## Asynchronous Exceptions

`throwTo :: Exception e => ThreadId -> e -> IO ()` throws an exception
to the specified thread. The most naive way of thread cancellation is
done in this manner.

## Masking Asynchronous Exceptions

* Deadlock might be caused by naive thread cancellation:
  when a thread is killed and it is holding some `MVar`s,
  the other threads waiting on these `MVar`s will block forever.

* `mask :: ((IO a -> IO a) -> IO b) -> IO b` accepts a function.
  Let's say `restore :: IO a -> IO a` is bound when entering
  the body of argument function. Then the thread will be masked
  except for the part where `restore` is called.
  A masked thread cannot receive asynchronous exceptions,
  so we can use this technique on critical part of our programs
  See also: [mask (base-4.8.2.0)](https://hackage.haskell.org/package/base-4.8.2.0/docs/Control-Exception.html#v:mask)

Note: the implementation of `bracket` actually provides a perfect example of it:

```haskell
bracket
        :: IO a         -- ^ computation to run first (\"acquire resource\")
        -> (a -> IO b)  -- ^ computation to run last (\"release resource\")
        -> (a -> IO c)  -- ^ computation to run in-between
        -> IO c         -- returns the value from the in-between computation
bracket before after thing =
  mask $ \restore -> do
    a <- before
    r <- restore (thing a) `onException` after a
    _ <- after a
    return r
```

In the body of `bracket`, only `restore`'s argument `thing a` can receive asynchronous
exceptions. And this ensures `before` and `after` are paired even when exceptions are raised.

* Even with the presense of `mask`, some operations like `takeMVar` are still interruptible until to the point where it returns.
  otherwise, while `takeMVar` is waiting, that thread cannot respond to asynchronous exceptions.
  (In general all operations that may block indefinitely are designated as interruptible.)

* Keep in mind that functions like `mask` or `uninterruptibleMask` are primitives.
  We can use higher-level combinators like `modifyMVar_` or `modifyMVar` to take care
  of these issues for us.
