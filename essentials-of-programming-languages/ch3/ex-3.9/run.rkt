#lang eopl

(require "../../common.rkt")
(require "../../test-utils.rkt")

(require "./top.rkt")
(require "./data-structures.rkt")

(out (run
  (string-append
    "let x = 4 "
    "in cons(x, "
    "        cons(cons(-(x,1), "
    "                  emptylist),  "
    "             emptylist)) ")))

(out (run
  (string-append
    "null?(emptylist)")))

; (test-all)
