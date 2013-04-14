(load "../common/utils.scm")

; * normal-order evaluation
;     fully expand and then reduce 
; * applicative-order evaluation
;     evaluate the arguments and then apply (interpreter actually uses)

; if procedure application can be modeled using subsitution
;     result of normal-order & applicative-order evaluation should be the same
