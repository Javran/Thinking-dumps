(load "./my-eval-e-define.scm")
(load "./amb-eval-test.scm")

(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (amb-analyze (definition-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)
               (define-variable! var val env)
               (succeed 'ok fail2))
             fail))))

(define (install-amb-define)

  (define analyze-define
    analyze-definition)

  (define (test)
    (let ((env (init-env)))
      (test-eval `(define x 1) env)
      (test-eval `(define y 2) env)

      (do-test
       test-eval
       (list
        (mat `x env 1)
        (mat `y env 2))
       (test-compare equal?))

      (test-eval `(define x 3) env)

      (do-test
       test-eval
       (list
        (mat `x env 3)
        (mat `y env 2))
       (test-compare equal?))

      'ok))

  (define handler
    (make-amb-handler
     'define
     analyze-define
     test))

  (ahandler-register! handler)
  'ok)

;; Local variables:
;; proc-entry: "./amb-eval.scm"
;; End:
