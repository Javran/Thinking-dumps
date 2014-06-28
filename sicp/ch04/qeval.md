# An Implementation of the Query System in SICP

## qeval.scm

Put together each components to form this system.

TODO: still working

* `(qeval-initialize!)`

    Clean up the database.

## qeval-stream.scm

Utilities for operation on streams

* `(stream-append-delayed <s1> <delayed-s2>)`

    Append stream `<s2>` to `<s1>`, where `<s2>` is the result
    of forcing `<delayed-s2>`.

* `(interleave-delayed <s1> <delayed-s2>)`

    Interleave two streams so that the elements from both streams
    will show up alternatively. When one of the stream has run out
    of element, the stream continues with all the rest elements in the
    other stream.

* `(stream-intermap <proc> <s>)`

    Similar to the stream version of `flatmap`, `<proc>` is applied to each element
    in the stream, and the resulting streams are interleaved together to form
    a single stream. `(<proc> <e>)` should produce a stream for each element in `<s>`.

* `(singleton-stream <x>)`

    Create a stream that has exactly one element: `<x>`.

## qeval-data-directed.scm

Implementation of data-directed scheme,
described in 2.4.3 of SICP.

The data are represented as a pair, whose `car` part is its type
and `cdr` part is its content. Two procedures `get` and `put` is used
to set up and retrieve the corresponding handler according to the tag (i.e. `car` part)
of any given data. The tag comparison is done by `eq?`.

* `(list-tagged-with <tag>)`

    `(list-tagged-with <tag>)` produces a predicate that tests if a given data is valid
    and tagged with a specified tag. (e.g. `((list-tagged-with 'foo) 'data)` produces `#f`,
    `((list-tagged-with 'foo) '(foo data))` produces `#t`).

* `(type <data>)` and `(contents <data>)`

    Type and content accessors, should be used on valid data.

* `(put <key1> <key2> <val>)`

    Put an arbitrary value (except `#f`) into the global lookup table.
    `<key1>` and `<key2>` could be arbitrary and the key comparison is done by `equal?`.

* `(get <key1> <key2>)`

    Retrieve arbitrary data (except `#f`) from the global lookup table.
    Returns `#f` if the value cannot be found.

* `(proc-table-initialize!)`

    Initialize data-directed scheme by removing all values from the global lookup table.

## qeval-frame.scm

Frames are like environments which keep track of variables and the values they are bound to.

* `empty-frame`

    The empty frame

* `(make-binding <var> <val>)`

    Make a binding pair (for internal use only, please use `extend` instead)

* `(extend <var> <val> <frame>)`

    Extend an existing frame with `<var>` being bound to `<val>`,
    return the resulting frame.

* `(binding-variable <binding>)` and `(binding-value <binding>)`

	Variable and value accessors used on the result of a successful `binding-in-frame` call.

* `(binding-in-frame <var> <frame>)`

    Lookup the frame using `<var>` as a key, return the binding with `<var>` as key on success,
    return `#f` on failure.


## qeval-base.scm

## qeval-compound-queries.scm

## qeval-database.scm

## qeval-driver-loop.scm

## qeval-filters.scm

## qeval-pattern.scm

## qeval-rules-and-unif.scm

## qeval-simple-query.scm

## qeval-transform.scm

## qeval-tests.scm
