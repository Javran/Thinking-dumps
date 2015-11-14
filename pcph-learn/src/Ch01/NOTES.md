## Notes for Chapter 1

### Parallelism and Concurrency

* **parallel** is more of using more computation power so we can
get results faster. It's usually deterministic (but still there are exceptions)

* **concurrency** usually involves multiple threads of controls.
Many parts that serves different purposes have to work together, interact with each other.
So concurrent programming is usually considered nondeterministic due to its nature.

## Fully automatic parallelization

``To make the program run faster, we have to gain more from parallelism
than we loose due to the overhead of adding it, and compile-time analysis
cannot make good judgments in this area''

## Parallel Haskell

* Good
    - an abstract framework, can work on a wide range of hardwares.
    - can take advantage of runtime system (TODO: I'm not sure why)
* Bad
    - the details of execution are hidden, this makes troubleshotting
      and fixing a little more obscure because we lack some controls.

* Things to be considered
    - Partition task wisely, so the overhead of using parallelism won't outweigh
      the benefit of using it.
    - Data dependency. Some data can be computed only when some others part of data
      are available.
