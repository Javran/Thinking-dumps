(load "../common/utils.scm")
(load "../common/test-utils.scm")

;; there are actually two kinds of normal-order evaluations:
;; * call-by-name (no memorization)
;; * call-by-need (memorization, lazy evaluation)
;; recall the difference between applicative-order and normal-order
;; is how an expression gets evaluated: before function application,
;; applicative-order evaluation evaluates all the arguments
;; while normal-order evaluation only evaluates a value as needed
;; there are two kinds of situations:
;; * a primitive function needs that value, in which case
;;   the evaluation is forced because the primitives have no
;;   knowledge about expressions
;; * a condition depends on that value, in which case
;;   we need the exact value not the unevaluated expression in order to
;;   proceed

;; * guess we first need to implement thunk?


(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
