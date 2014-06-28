# An Implementation of the Query System in SICP

## qeval.scm

Put together each components to form this system.

TODO: still working

* `qeval-initialize!`

    clean up the database.

## qeval-stream.scm

Utilities for operation on streams

* `stream-append-delayed <s1> <delayed-s2>`

    append stream `<s2>` to `<s1>`, where `<s2>` is the result
    of forcing `<delayed-s2>`.

* `interleave-delayed <s1> <delayed-s2>`

    interleave two streams so that the elements from both streams
    will show up alternatively. When one of the stream has run out
    of element, the stream continues with all the rest elements in the
    other stream.

* `stream-intermap <proc> <s>`

    similar to the stream version of `flatmap`, `<proc>` is applied to each element
    in the stream, and the resulting streams are interleaved together to form
    a single stream. `(<proc> <e>)` should produce a stream for each element in `<s>`.

* `singleton-stream <x>`

    create a stream that has exactly one element: `<x>`.

## qeval-base.scm

## qeval-compound-queries.scm

## qeval-database.scm

## qeval-driver-loop.scm

## qeval-filters.scm

## qeval-frames.scm

## qeval-data-directed.scm

## qeval-pattern.scm

## qeval-rules-and-unif.scm

## qeval-simple-query.scm

## qeval-transform.scm

## qeval-tests.scm
