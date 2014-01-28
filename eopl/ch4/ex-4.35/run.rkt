#lang eopl

(require "../../common.rkt")
(require "../../test-utils.rkt")

(require "./top.rkt")
(require "./data-structures.rkt")

(test-all)

; What are the expressed and denoted values of this language?
;
; By introducing "ref", reference becomes a valid value of ExpVal:
; ExpVal = Int + Bool + Proc + Ref(ExpVal)
; DenVal = Ref(ExpVal)
