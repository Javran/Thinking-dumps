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

- One comment in book: "Exceptions in Haskell can be caught, but only in IO monad.".
  When programming in Haskell, I have never found this observation. Probably because that
  the type of `catch` and some other Exception-related functions prevents us from doing something
  wrong in this respect.

# Error Handling with Async

- Nothing fancy. We are just turning `data Async a = MVar a` into `data Async a = MVar (Either SomeException a)`.
  So by the time when there is an exception, we will wrap it in the result.

# Merging

- Sometimes we might want each of separate actions to put their results in the same `MVar`.
  By doing so the first completed action will take over `MVar` first and put the result in it.

- Because all actions have to their results in the same `MVar a`, `a` has to be a sum type.
  In the book `waitEither` merges 2 events by lifting them into `Either`, and `waitAny` just
  require all results to be of type `a`, leaving the resposibility of unifying them to users.
