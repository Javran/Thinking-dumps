# Notes on Chapter 7

* Threads can be created using `forkIO`
* `MVar a`, where `a` is an arbitrary type,
  can be thought as a box that can hold at most one value of type `a` in it.

* Threads communicates through `MVar`s, calls related to `MVar` are usually blocked
  when the required operation (read or write) is not possible at the moment.
  And threads will be woke up as soon as the required operation becomes possible.
  (e.g. `takeMVar` and `putMVar`)

* "Haskell's philosophy is to provide a set of very simple
  but general features that you can use to build higher-level functionality."
  This is also true for concurrent programming. In this chapter we have seen
  building various kind of things using `MVar` including:

    * sharing values / passing messages among threads
    * implementing unbounded channel

* When multiple threads are waiting on `MVar`, GHC runtime has a simple way to guarantee
  some properties of fairness: "No thread can be blocked indefinitely on an MVar unless
  another thread holds that MVar indefinitely". This is not a perfect approach,
  as it does not guarantee CPU time are shared equally among threads. But it works fine
  in practice.

* `stdout` handle is represented by `MVar`s. So only one thread can have it
  and do opreations on it.
  (NOTE: looking at the definition of `Handle`, you'll find they are indeed represented
  by MVars, so this is probably true not just for `stdout`, but also for all kinds of handlers)
