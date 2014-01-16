#lang eopl

(require "../../common.rkt")
(require "../../test-utils.rkt")

(require "./top.rkt")
(require "./data-structures.rkt")

(test-all)

; I guess the not expressible thing
;   is the side effect that "print" has.
