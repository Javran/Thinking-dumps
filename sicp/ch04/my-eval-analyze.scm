;; Local variables:
;; proc-entry: "./my-eval.scm"
;; End:

(define (my-analyze exp)
  ;; a simple form is either a self-evaluating s-exp
  ;; or a variable that needs to be looked up from the env
  (define (try-simple-analyze exp)
    (cond ((self-evaluating? exp)
           (just (const exp)))
          ((variable? exp)
           (just
            (lambda (env)
              (lookup-variable-value exp env))))
          (else nothing)))

  ;; try to dispatch the s-exp according to slots
  (define (try-dispatch-analyze exp)
    (if (non-empty? exp)
        ;; try to fetch the handler
        (let ((handler (my-eval-get (car exp))))
          (if handler
              (just
               (handler-analyze handler exp))
              nothing))
        nothing))

  ;; try application
  (define (try-app-analyze exp)
    (if (application? exp)
        (just
         (lambda (env)
           (my-apply
            (my-eval (operator exp) env)
            (list-of-values (operands exp) env))))
        nothing))
  ((maybe
    ;; if the analysis goes well, return the result
    identity
    ; else there must be some errors
    (lambda ()
      (error "unknown expression:" exp)))
   (or (try-simple-analyze exp)
       (try-dispatch-analyze exp)
       (try-app-analyze exp)
       nothing)))

;; an analyze-xxx is a procedure of form `(analyze-xxx exp)`
;; this procedure turns an analyzer into a `(eval-xxx exp env)`
(define (analyze->eval analyze-xxx)
  (define (eval-xxx exp env)
    ((analyze-xxx exp) env))
  eval-xxx)

(define my-eval-analyze
  (analyze->eval my-analyze))
