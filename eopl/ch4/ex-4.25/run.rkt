#lang eopl

(require "../../common.rkt")
(require "../../test-utils.rkt")

(require "./top.rkt")
(require "./data-structures.rkt")
(require "./printer.rkt")

; based on ex 4.22

(test-all)

; does the scope of a variable include the
;   initializer for variables declared in the same block statement?
; no, in my implementation all variables in the same `var-stmt`
;   is initialized "simutaneously" using the same environment
; see also: `test-var-assign-3` in `./tests.rkt`
