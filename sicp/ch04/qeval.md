# An Implementation of the Query System in SICP

## qeval.scm

Put together each components to form this system.

TODO: still working

* `(qeval-initialize!)`

    Clean up the database.

## qeval-utils.scm

Some miscellaneous functions.

* `(set-equal? <s1> <s2>)`

    Test set equality of two data, ordering doesn't matter.

* `(result-frame-equal? <r1> <r2>)`

    Test whether two resulting frames are equal.
    This predicate holds only if `<r1>` and `<r2>` are both `failed` symbols
    or if frame `<r1>` and frame `<r2>` contain same set of variable-value bindings.

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

* `(alist->frame <data>)`

    Convert from an alist (whose elements are all key-value pairs) to a frame.

## qeval-syntax-trans.scm

Query syntax transformation between internal and external representation.

To better distinct constant symbols from variables that might be bound to some
values laterly, the internal representation is slightly different from the external one.

Symbols like `'foo` will be transformed to `'(? foo)`. When doing rule application,
`'(? foo)` can be further transformed to `'(? 10 foo)`
(this is also a valid internal representation), where `10` is a number guaranteed
to be unique to avoid confusion between different applications.

The transformation also has benefits that we can tell if a given symbol is
a constant symbol or a variable by simple predicates.

* `(query-syntax-process <exp>)`

    Transform an expression into its internal representation. This is done
    by transforming each variable to a list tagged with `'?`.
    (e.g. `'(job ?x ?y symb)` will be transformed to `'(job (? x) (? y) symb)`)

* `(var? <data>)` and `(constant-symbol? <data>)`

    Test if a given data is a variable or a constant symbol (internal representation)
    (e.g. `(var? '(? var))` produces `#t`, `(constant-symbol? 'symb)` produces `#t`,
    `(var? 'symb)` produces `#f` and `(constant-symbol? '(? var))` produces `#f`)

* `(new-rule-application-id!)`

    Generate unique numbers for rule applications when called.

* `(make-new-variable <var> <id>)`

    Make new variable based on an internal variable `<var>`, with
    `<id>` as its identifier. A typical usage is like:
    `(make-new-variable (? var) (new-rule-application-id!))`
    which might produce `'(? 1 var)`, where `1` can be arbitrary.

* `(contract-question-mark <var>)`

    Transform internal variable to its external representation.
    (e.g. `(contract-question-mark (? foo))` produces `'?foo`,
    `(contract-question-mark (? 2 foo))` produces `?foo-2`)

## qeval-database.scm

Query system database management. Defines formats for assertions and rules.
Specifies how assertions and rules are stored in this system.

In theory, merely using two streams: `THE-ASSERTIONS` and `THE-RULES`
will also work, since pattern matching or unification will be following
to eliminate invalid solutions, but we can index some of the stored
assertions and rules to speed up this part a little bit.

### Assertions

Assertions are represented and stored as pairs, whose `car` part is
not equal to the symbol `'rule`. The following data are all valid assertions:

* `'(foo bar)`
* `'(foo)`
* `'(user (address (10 0 0 1)) (tel 123456))`
* `'(can be improper . list)`

All assertions are stored as it is on the stream `THE-ASSERTIONS`.
In addition, assertion that begins with constant symbol will also be stored
to a global table. With the constant symbol as the first index and
symbol `'assertion-stream` as the second index.
(The global table used for data directed programming is being reused here.)

### Rules

Rules are lists of 2 or 3 elements with `'rule` as their `car` part.
The second element is the conclusion. The third element is optional and
defaults to `'(always-true)` (which stands for an always-true rule as
its name suggests). The rule body part (namely the third element)
can contain complex queries and will be handled by either simple handler or
handlers stored in the global table according to the query itself.

Like assertions, `THE-RULES` holds all rules in the system.
Rules that begin with a constant symbol or a variable are
also stored in a global table. Rules that begins with a constant symbol
will be stored in a stream whose first index is the constant symbol,
and second index is the symbol `'rule-stream`. Rules that begins with
a variable will be stored into a stream with its first index begin `'?`
and second index `'rule-stream`.

### Definitions

* `(rule? <data>)`

    Test whether the data is a rule

* `(conclusion <data>)` and `(rule-body <data>)`

    Accessors for rules

* `(assertion-to-be-added? <exp>)`

    Tests if an internal expresion is a command of adding
    an assertion or a rule. (e.g. `(assert! (what (an assertion)))`)

* `(add-assertion-body <exp>)`

    Access the assertion / rule of the `assert!` expression.

* `(use-index? <data>)`

    Test if the assertion / rule uses constant symbol as its `car` part.
    These assertions / rules can be indexed.

* `(index-key-of <data>)`

    (Internal use only) Calculate the proper first index for assertions and rules.

* `(get-stream <key1> <key2>)`

    A wrapper that fetchs data from the global table using two indices.
    It return value is guaranteed to be a proper stream for adding assertions / rules.

* `THE-ASSERTIONS`

    The stream that stores all the assertions.

* `(fetch-assertions <pattern> <frame>)`

    Fetch a stream of assertions, `<pattern>` and `<frame>` are used
    as hints to narrow down the size of the stream. For now `<frame>`
    is not used.

* `THE-RULES`

    The stream that stores all the rules.

* `(fetch-rules <pattern> <frame>)`

    Fetch a stream of rules, `<pattern>` and `<frame>` are used
    as hints to narrow down the size of the stream. For now `<frame>`
    is not used.

* `(add-rule-or-assertion! <assertion>)`

    Add either a rule or an assertion to the system. (Recall that
    rule is of form `(rule <conclusion> [body])` and all the other
    forms are regarded as assertions)

## qeval-pattern-matching.scm

Pattern matching algorithm.

* `(pattern-match <pat> <dat> <frame>)`

    Pattern matching between `pat` and `dat`,
    `dat` shouldn't contain variables.
    Returns the extended frame with variables in the pattern
    bound to proper values. If the pattern matching isn't possible
    the symbol `'failed` will be returned instead.

* `(extend-if-consistent <var> <dat> <frame>)`

    Try to extend `<var>` to match against `<dat>`.
    `dat` shouldn't contain variables.
    Returns the extended frame with variables in the pattern
    bound to proper values. If the pattern matching isn't possible
    the symbol `'failed` will be returned instead.

* `(check-an-assertion <assert> <qpat> <qframe>)`

    Check if the pattern can be matched against the assertion
    with the current variable bindings in `<frame>`.
    The return value is always a stream containing only vaild
    extended frames.

* `(find-assertions <pat> <frame>)`

    Try to do pattern matching against related assertions,
    return a stream of valid extended frames.

## qeval-unification.scm

## qeval-handlers.scm

## qeval-driver-loop.scm

## qeval-tests.scm
