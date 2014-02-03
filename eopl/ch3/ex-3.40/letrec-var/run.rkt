#lang eopl

(require "../../../common.rkt")
(require "../../../test-utils.rkt")

(require "./top.rkt")
(require "./data-structures.rkt")

      (run
        "letrec f(x) = if zero?(x)  then 0 else -((f -(x,1)), -2) in (f 4)"
        )
; (test-all)
