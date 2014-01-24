; handle lambda expressions

(define (install-eval-lambda)

  (define eval-lambda 'todo)
  (define test 'todo)

  (define handler
    (make-handler
      'lambda
      eval-lambda
      test))

  (handler-register! handler)
  'ok)
