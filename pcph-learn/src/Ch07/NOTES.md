# Notes for Chapter 7

* Threads can be created using `forkIO`
* `MVar a`, where `a` is an arbitrary type,
  can be thought as a box that can hold at most one value of type `a` in it.

* Threads communicates through `MVar`s, calls related to `MVar` are usually blocked
  when the required operation (read or write) is not possible at the moment.
  And threads will be woke up as soon as the required operation becomes possible.
  (e.g. `takeMVar` and `putMVar`)

* When multiple threads are waiting on `MVar`, GHC runtime has a simple way to guarantee
  some properties of fairness: "No thread can be blocked indefinitely on an MVar unless
  another thread holds that MVar indefinitely". This is not a perfect approach,
  as it does not guarantee CPU time are shared equally among threads. But it works fine
  in practice.
