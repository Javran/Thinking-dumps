# Notes on Chapter 9

## Asynchronous Exceptions

`throwTo :: Exception e => ThreadId -> e -> IO ()` throws an exception
to the specified thread. The most naive way of thread cancellation is
done in this manner.
