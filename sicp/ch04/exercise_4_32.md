# Difference

`cons-stream` is strict in the left part but lazy in the right part.
When using lazy evaluation strategy, `cons` will be lazy in all its arguments,
which means the compuation of the first argument don\'t have to be done immediately
after its construction.

# How to take advantage

Suppose we have a stream that every element in it contains
a costly computation. While `cons-stream` need to evaluate
its first argument to construct a lazy pair, lazy `cons` does not.
This suggest that totally lazy pairs might offer a slightly quicker time
to be constructed.

As an example, `costly` is a function that aceepts an integer and returns
something that involves costly computation which is dependent on the argument:

    (define (costly x)
      ...)

In an evaluator that uses eager evaluation, we have `cons-stream`:

    (define results
      (cons-stream
        (costly 1)
        (cons-stream
          (costly 2)
          ...)))

In this example, `(costly 1)` will be evaluated at the time `results` are created.

And if there is an evaluator that uses lazy evaluation strategy,
we will have:

    (define results
      (cons
        (costly 1)
        (cons
          (costly 2)
          ...)))

Despite that they look almost the same, but `(costly 1)` will not be evaluated
unless it is forced somewhere else.
