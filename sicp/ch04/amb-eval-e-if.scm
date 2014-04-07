(load "./my-eval-e-if.scm")
(define (install-amb-if)

  (define (analyze-if exp)
    (let ((pproc (amb-analyze (if-predicate exp)))
          (cproc (amb-analyze (if-consequent exp)))
          (aproc (amb-analyze (if-alternative exp))))
      (lambda (env succeed fail)
        (pproc env
               ;; do evluation (on analyzed data)
               ;; obtain pred-value.
               (lambda (pred-value fail2)
                 (if (true? pred-value)
                     (cproc env succeed fail2)
                     (aproc env succeed fail2)))
               fail))))

  (define test #f)

  (define handler
    (make-amb-handler
     'if
     analyze-if
     test))

  (ahandler-register! handler)
  'ok)

;; Local variables:
;; proc-entry: "./amb-eval.scm"
;; End:
