#lang eopl

(require "../../common.rkt")
(require "../../test-utils.rkt")

(require "./top.rkt")
(require "./data-structures.rkt")

(out (expval->num (run
  (string-append
    "let x = add(100,20) in "
      "let y = mul(3,19) in "
        "quotient(x,y)"))))

(out (quotient (+ 100 20) (* 3 19)))
