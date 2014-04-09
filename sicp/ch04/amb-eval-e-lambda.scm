(load "./my-eval-e-lambda.scm")
(load "./amb-eval-e-begin.scm")

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
    (out "testing lambda...")
    (out
     (amb-eval `((lambda (x) (+ x x)) 20)
               (init-env)
               (lambda (exp fail) exp)
               (lambda () #f)))
    'todo)

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
