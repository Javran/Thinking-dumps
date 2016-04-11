# Notes on Chapter 8

## Asynchronous communication

As we have experimented in the previous chapter. `MVar a` can be used
as a means of communicating between threads:

- we create an empty `MVar a`, allow one thread to perform the operation asynchronously
  and put the result value on that `MVar`.

- meanwhile, other threads that have access to the `MVar` can wait on the result.

Keep in mind that in order to (probably) wait and finally get a result for all threads
waiting on the same `MVar`, it is important to use `readMVar` instead of `takeMVar`.
Because `takeMVar` reads the value but won't put it back, so only one thread can
get a result while all other waiting threads can't.

## Exceptions in Haskell

- One must give explicit type on what exception should be caught.

- Exceptions form a hierarchy, at the top of the hierarchy is a type called `SomeException`.
  If we specify exception to be of this type, then we will end up catching all exceptions
  in the context.

- We can implement our own exception type by making it an instance of `Exception` typeclass.
  (but the book doesn't seem to give anything about what exactly does the exception
   hierarchy looks like or how to make use of the hierarchy). (TODO)
