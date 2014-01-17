#lang eopl

(require "../../common.rkt")
(require "../../test-utils.rkt")

(require "./top.rkt")
(require "./data-structures.rkt")

; * syntax change: -(x,y) => (diff x y)
;                  zero?(x) => (zero? x)
; * introduce primitive procedure
; * inject initial environment to insert primitives

(test-all)
