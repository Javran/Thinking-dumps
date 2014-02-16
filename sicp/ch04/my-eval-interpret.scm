; Local variables:
; proc-entry: "./my-eval.scm"
; End:

(define (my-eval-interpret exp env)
  ; `try-xxx` are all supposed to return:
  ; either `(list <value>)` or `#f`
  (define (eval-succeeded? result)
    result)
  (define (result->val result)
    (car result))
  (define (val->result val)
    (list val))

  ; try simple form evaluation
  (define (try-simple-eval exp env)
    (cond ((self-evaluating? exp)
            (val->result exp))
          ((variable? exp)
            (val->result
              (lookup-variable-value exp env)))
          (else #f)))

  ; try to dispatch according to slot (i.e. the tag)
  (define (try-dispatch-eval exp env)
    (if (non-empty? exp)
      ; try to fetch the handler
      (let ((handler (my-eval-get (car exp))))
        (if handler
          (val->result
            (handler-eval handler exp env))
          #f))
      #f))

  ; try application
  (define (try-app-eval exp env)
    (if (application? exp)
      (val->result
        (my-apply
          (my-eval (operator exp) env)
          (list-of-values (operands exp) env)))
      #f))

  (let ((result
          (or (try-simple-eval   exp env)
              (try-dispatch-eval exp env)
              (try-app-eval      exp env))))
    (if result
      (result->val result)
      (error "unknown expression:" exp))))
