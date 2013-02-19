## the implementation of some of the commonly used macros in Clojure

Most of the commonly used macros can be found [here](http://clojure.org/macros)
and their source codes are located [here](https://github.com/clojure/clojure/blob/master/src/clj/clojure/core.clj)(newest one)
which is the source code of `clojure.core`, search `defmacro` in page so you can find their implementations.

## an example of defining your own lazy sequence

Please refer to `clojure-codes.day-2.find.find/problem_3n_plus_1`
which demonstrates how to describe 3n+1 problem via infinity(or, say lazy) sequence.

## find the current status of defrecord and protocol

Let's check the commit history of `core_deftype` [here](https://github.com/richhickey/clojure/commits/master/src/clj/clojure/core_deftype.clj)
that's where the definition of `defprotocol` and `defrecord` be placed in.

The last update was at Jun 07, 2010

it's been relatively stable I think (but the document string of `clojure.core/defrecord` still saying "Alpha - subject to change")
