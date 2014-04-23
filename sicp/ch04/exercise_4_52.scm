(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./amb-eval.scm")

(define (install-amb-if-fail)

  ;; (if-fail <try-body> <fallback value>)

  (define try-body cadr)
  (define fallback-val caddr)

  (define (analyze-if-fail exp)
    (let ((try-body-a (amb-analyze (try-body exp)))
          (fallback-val-a (amb-analyze (fallback-val exp))))
      (lambda (env succeed fail)
        (try-body-a
         env
         succeed
         (lambda ()
           (fallback-val-a
            env
            succeed
            fail))))))

  (define (test)
    'todo)

  (define handler
    (make-amb-handler
     'if-fail
     analyze-if-fail
     test))

  (ahandler-register! handler)
  'ok)

(install-amb-if-fail)

(run-all-slot-tests)

(out (amb-eval-all `(if-fail (amb 1 2 3) 'ok) (init-env)))

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
