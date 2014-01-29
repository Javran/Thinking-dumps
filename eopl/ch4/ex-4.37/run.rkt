#lang eopl

(require "../../common.rkt")
(require "../../test-utils.rkt")

(require "./top.rkt")
(require "./data-structures.rkt")

; write a program that produces different answers
;   using call-by-result-value and call-by-reference:
; see testcases: `different-result-1` and `different-result-2`
;   in ./tests.rkt

(test-all)
