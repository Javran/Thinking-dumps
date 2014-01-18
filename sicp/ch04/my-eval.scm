(load "../common/utils.scm")
(load "../common/test-utils.scm")

; my `eval` and `apply` implementaton.
(define *my-eval-do-test* #t)

(load "./my-eval-handler.scm")
(load "./my-eval-data-directed.scm")

(load "./my-eval-e-simple.scm")

; lookup-variable-value: not implemented

(define (my-eval exp env)
  (cond ((self-evaluating? exp)
          exp)
        ((variable? exp)
          (lookup-variable-value exp env))
        ; begin data dispatching here

        ; none of above, it should be an application
        ))
