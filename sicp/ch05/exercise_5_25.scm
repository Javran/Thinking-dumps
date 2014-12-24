(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./exercise_5_25_common.scm")

;; there are actually two kinds of normal-order evaluations:
;; * call-by-name (no memorization)
;; * call-by-need (memorization, lazy evaluation)
;; recall the difference between applicative-order and normal-order
;; is how an expression gets evaluated: before function application,
;; applicative-order evaluation evaluates all the arguments
;; while normal-order evaluation only evaluates a value as needed
;; there are three kinds of situations:
;; * a primitive function needs that value, in which case
;;   the evaluation is forced because the primitives have no
;;   knowledge about expressions
;; * a condition depends on that value, in which case
;;   we need the exact value not the unevaluated expression in order to
;;   proceed
;; * a delayed value is a function and we need that function in order to
;;   proceed

;; * guess we first need to implement thunk?
;; * few parts are needed to be modified:
;;   + function application (need to force the operator)
;;   + force all expressions before passing it to primitives
;;   + use delayed values for compound functions
;;   + force the condition expression value when dealing with "if" expressions
;;   + careful when dealing with derived forms like "cond",
;;     if later on we want to implement it independently instead of converting it
;;     to a nested if-form, we need to at least leave a warning

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
