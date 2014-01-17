#lang eopl

(require "../../common.rkt")
(require "../../test-utils.rkt")

(require "./top.rkt")
(require "./data-structures.rkt")

; now `equal?` `greater?` and `less?` wind up in `bool-exp`

(test-all)
