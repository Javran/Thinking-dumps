(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "simu.scm")
(load "simu_ec_patch.scm")

(load "exercise_5_23_common.scm")
(load "exercise_5_25_machine.scm")

;; here we need a lazy evaluator to verify
;; that things would happen in a different
;; order than applicative-order evaluation
;; the default one isn't lazy. so we just run
;; test codes using our interpreter and observe
;; the outputs.

(do-test
 (lambda (exp)
   (machine-eval exp (init-env)))
 (list
  (mat `(begin
          (define (f x)
            (lambda (y)
              (lambda (z)
                (* (- x y)
                   (+ 20 z)))))
          ;; (100 - 70) * (20 + 5) = 30 * 25 = 750
          (((f 100) 70) 5))
       750)
  (mat `(begin
          (define a 10)
          (define (f x) 20)
          ;; the value of the argument
          ;; is never evaluated here
          ;; so "a" is always 10
          (f (begin
               (set! a 20)
               100))
          a)
       10)
  (mat `(begin
          (define a 0)
          (define (f x y z)
            ;; the value of "z" and "x" gets forced
            ;; but not "y"
            (+ z x))
          (f (begin
               (set! a (+ (* a 10) 1))
               1)
             (begin
               (set! a (+ (* a 10) 2))
               2)
             (begin
               (set! a (+ (* a 10) 3))
               3))
          a)
       31)
  ))

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
