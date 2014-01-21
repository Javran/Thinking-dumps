(define (install-eval-set!)

  (define assignment-variable cadr)
  (define assignment-value   caddr)

  (define (eval-set! exp env)
    (set-variable-value!
      (assignment-variable exp)
      (my-eval (assignment-value exp) env)
      env)
    'ok)

  (define (test)
    'todo)
  
  (define handler
    (make-handler
      'set!
      eval-set!
      test))

  (handler-register! handler)
  'done)
