(load "./my-eval-e-cond.scm")

(define (install-amb-cond)

  (define (analyze-cond exp)
    (amb-analyze (cond->if exp)))

  (define (test)
    'todo)

  (define handler
    (make-amb-handler
     'cond
     analyze-cond
     test))

  (ahandler-register! handler)
  'ok)
