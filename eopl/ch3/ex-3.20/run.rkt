#lang eopl

(require "../../common.rkt")
(require "../../test-utils.rkt")

(require "./top.rkt")
(require "./data-structures.rkt")

; no actual modification is done, so just skip the tests
; (test-all)

(out
  (expval->num
    (run
      "let f = proc (x)
                 proc (y)
                   -(x,-(0,y))
       in ((f 3) 4)")))
; 7
