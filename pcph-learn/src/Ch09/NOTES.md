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
