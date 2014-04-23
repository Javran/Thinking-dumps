(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./amb-eval.scm")

(define (install-amb-if-fail)

  ;; (if-fail <try-body> <fallback value>)

  (define try-body cadr)
  (define fallback-val caddr)

  ;; TODO: is this esentially "(amb (amb 1 2) 'failback)"?
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
    (let ((env (init-env)))
      (do-test
       amb-eval-all
       (list
        ;; test the failback value
        (mat `(if-fail 1 'ok) env '(1 ok))
        (mat `(if-fail (amb 1 2 3) 4) env '(1 2 3 4))))
      'ok))

  (define handler
    (make-amb-handler
     'if-fail
     analyze-if-fail
     test))

  (ahandler-register! handler)
  'ok)

(install-amb-if-fail)

(run-all-slot-tests)

(define (test-prog l)
  `(if-fail (let ((x (amb (quote ,@l))))
              (define (require x)
                (if x 'pass (amb)))
              (require (even? x))
              x)
            'all-odd))

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
