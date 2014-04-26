(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./amb-eval.scm")

(define require-predicate cadr)

(define (install-amb-require)

  (define (analyze-require exp)
    (let ((pproc (amb-analyze (require-predicate exp))))
      (lambda (env succeed fail)
        (pproc env
               (lambda (pred-value fail2)
                 (if (not pred-value)
                     ???
                     (succeed 'ok fail2)))
               fail))))

  (define (test)
    'todo)

  (define handler
    (make-amb-handler
     'require
     analyze-require
     test))

  (ahandler-register! handler)
  'ok)

(install-amb-require)

(run-all-slot-tests)

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
