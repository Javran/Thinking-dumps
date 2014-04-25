(load "./my-eval-e-cond.scm")
(load "./my-eval-e-begin.scm")

(define (install-amb-cond)

  (define (analyze-cond exp)
    (amb-analyze (cond->if exp)))

  (define (test)
    (define env
      (init-env))

    (define cond-test-exp-1
      `(cond ((= a 0) 2)
             ((= a 1) 1)
             ((= a 2) 0)))

    (define cond-test-exp-2
      `(cond ((= a 0) 10)
             (else 10 15 20)))

    (define cond-test-exp-3
      ;; test arrow extension
      `(cond ((= a 0) => (lambda (x) (if x 10 20)))
             ((= a 1) => (lambda (x) (if x 30 40)))
             ((= a 2) 50)
             (else 60)))
    (do-test
     amb-eval-all
     (list
      (mat cond-test-exp-1
           (extend-environment
            '(a) '(0) env) 2)
      (mat cond-test-exp-1
           (extend-environment
            '(a) '(1) env) 1)
      (mat cond-test-exp-1
           (extend-environment
            '(a) '(2) env) 0)
      (mat cond-test-exp-2
           (extend-environment
            '(a) '(0) env) 10)
      (mat cond-test-exp-2
           (extend-environment
            '(a) '(1) env) 20)
      ;; test arrow extension
      (mat cond-test-exp-3
           (extend-environment
            '(a) '(0) env) 10)
      (mat cond-test-exp-3
           (extend-environment
            '(a) '(1) env) 30)
      (mat cond-test-exp-3
           (extend-environment
            '(a) '(2) env) 50)
      (mat cond-test-exp-3
           (extend-environment
            '(a) '(3) env) 60)
      )
     ;; different from its "my-eval" counterpart
     ;; "amb-eval-all" always return a list of possible lists
     ;; which we need to wrap the result in order to do the comparison
     (lambda (expected actual)
       (equal? expected (list actual)))
     )
    'ok)

  (define handler
    (make-amb-handler
     'cond
     analyze-cond
     test))

  (ahandler-register! handler)
  'ok)
