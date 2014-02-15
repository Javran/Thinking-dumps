#lang eopl

(require "../../common.rkt")
(require "../../test-utils.rkt")

(require "./top.rkt")
(require "./data-structures.rkt")

(test-all)

; make lambda arount apply-cont
;   requires just few lines of modification
;   The contract will become:
;     apply-cont : Cont * ExpVal -> Bounce
;   we can leave other stuffs unchanged,
;   because the initial call of the program
;   is `value-of/k` which eventually leads to
;   `apply-cont`, whose result is captured
;   and eventually "released" by `trampoline`.
