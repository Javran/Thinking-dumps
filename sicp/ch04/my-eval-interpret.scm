; Local variables:
; proc-entry: "./my-eval.scm"
; End:

(define (my-eval-interpret exp env)

  ; try simple form evaluation
  (define (try-simple-eval exp env)
    (cond ((self-evaluating? exp)
            (just exp))
          ((variable? exp)
            (just
              (lookup-variable-value exp env)))
          (else nothing)))

  ; try to dispatch according to slot (i.e. the tag)
  (define (try-dispatch-eval exp env)
    (if (non-empty? exp)
      ; try to fetch the handler
      (let ((handler (my-eval-get (car exp))))
        (if handler
          (just
            (handler-eval handler exp env))
          nothing))
      nothing))

  ; try application
  (define (try-app-eval exp env)
    (if (application? exp)
      (just
        (my-apply
          (my-eval (operator exp) env)
          (list-of-values (operands exp) env)))
      nothing))

  ((maybe
    ; if evaluation is succeeded,
    ;   simply return the result.
    identity
    ; else we raise an error
    (lambda ()
      (error "unknown expression:" exp)))
   ; try all possible eval
   ;   here special form `or`
   ;   makes it a `First Monoid`
   (or (try-simple-eval   exp env)
       (try-dispatch-eval exp env)
       (try-app-eval      exp env)
       nothing)))

