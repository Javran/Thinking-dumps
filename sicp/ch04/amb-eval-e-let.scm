(load "./my-eval-e-let.scm")
(load "./amb-eval-test.scm")

(define (install-amb-let)

  (define (analyze-let exp)
    (amb-analyze (let->combination exp)))

  (define (test)
    (let ((env (init-env)))
      
      'todo))

  (define handler
    (make-amb-handler
     'let
     analyze-let
     test))

  (ahandler-register! handler)
  'ok)
