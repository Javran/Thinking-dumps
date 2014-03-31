(load "./my-eval-e-quote.scm")
;; simple expressions include:
;; * self-evaluating
;; * quoted
;; * variables

(define (analyze-self-evaluating exp)
  (lambda (env succeed fail)
    (succeed exp fail)))

(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env succeed fail)
      (succeed qval fail))))

(define (analyze-variable exp)
  (lambda (env succeed fail)
    (succeed (lookup-variable-value exp env)
             fail)))

;; some simple tests
(define (def-succeed val fail)
  (format #t
          "succeeded with output: ~A~%"
          val))

((analyze-self-evaluating '1234) (init-env) def-succeed nil)

((analyze-quoted '(quote (this is a test))) (init-env) def-succeed nil)

((analyze-variable 'car) (init-env) def-succeed nil)

;; Local variables:
;; proc-entry: "./amb-eval.scm"
;; End:

