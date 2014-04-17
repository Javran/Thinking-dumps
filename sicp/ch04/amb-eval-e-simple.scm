(load "./my-eval-e-simple.scm")
;; simple expressions include:
;; * self-evaluating
;; * variables

(define (analyze-self-evaluating exp)
  (lambda (env succeed fail)
    (succeed exp fail)))

(define (analyze-variable exp)
  (lambda (env succeed fail)
    (succeed (lookup-variable-value exp env)
             fail)))

;; Local variables:
;; proc-entry: "./amb-eval.scm"
;; End:
