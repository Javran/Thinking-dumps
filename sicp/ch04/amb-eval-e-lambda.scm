(load "./my-eval-e-lambda.scm")
(load "./amb-eval-e-begin.scm")
(load "./amb-eval-test.scm")

(define (install-amb-lambda)

  (define (analyze-lambda exp)
    ;; read var names
    ;; and analyze body (sequence)
    (let ((vars (lambda-parameters exp))
          (bproc (analyze-sequence (lambda-body exp))))
      (lambda (env succeed fail)
        ;; on success, make the procedure.
        (succeed (make-analyzed-procedure vars bproc env)
                 fail))))

  (define (test)
    (let ((env (init-env)))
      (do-test
       test-eval
       (list
        (mat `((lambda (x) (+ x x)) 20) env 40)
        (mat `((lambda (x y) (x y))
               (lambda (x) (+ x 20))
               30) env 50)
        )
       (test-compare equal?)))
    'ok)

  (define handler
    (make-amb-handler
     'lambda
     analyze-lambda
     test))

  (ahandler-register! handler)
  'ok)

;; Local variables:
;; proc-entry: "./amb-eval.scm"
;; End:
