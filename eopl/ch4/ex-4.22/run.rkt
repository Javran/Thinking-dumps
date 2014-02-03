#lang eopl

(require "../../common.rkt")
(require "../../test-utils.rkt")

(require "./top.rkt")
(require "./data-structures.rkt")
(require "./printer.rkt")

(test-all)
(newline)

(print-to-screen #t)

(out "Example 1")
(run-one 'example-ex-4-22-1)

(out "Example 2")
(run-one 'example-ex-4-22-2)

(out "Example 3")
(run-one 'example-ex-4-22-3)

(out "Example 4")
(run-one 'example-ex-4-22-4)
