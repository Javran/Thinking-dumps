(load "./amb-eval-test.scm")

(define amb-choices cdr)

(define (analyze-amb exp)
  (let ((cprocs (map amb-analyze (amb-choices exp))))
    (lambda (env succeed fail)
      (define (try-next choices)
        (if (null? choices)
            (fail)
            ((car choices)
             env
             succeed
             (lambda ()
               (try-next (cdr choices))))))
      (try-next cprocs))))

(define (install-amb-amb)

  ;; analyze-amb is defined outside

  (define (test)
    (let ((env (init-env)))
      (do-test
       test-eval
       (list
        (mat `(amb) env 'failure)
        (mat `(amb 1 2 3) env 1)
        (mat `(amb "a" "b") env "a")
        (mat `(amb 'c 'd 'e) env 'c)
        )
       (test-compare equal?))
      ;; test using amb-eval-all
      (do-test
       amb-eval-all
       (list
        (mat `(amb) env '())
        (mat `(amb 1 2 3) env '(1 2 3))
        (mat `(amb "a" "b") env '("a" "b"))
        (mat `(amb 'c 'd 'e) env '(c d e))
        (mat `(cons (amb 1 2) (amb 3 4)) env
             '((1 . 3) (1 . 4) (2 . 3) (2 . 4)))
        )))
    'ok)

  (define handler
    (make-amb-handler
     'amb
     analyze-amb
     test))

  (ahandler-register! handler)
  'ok)
