(load "./my-eval-e-if.scm")
(define (install-amb-if)

  (define (analyze-if exp)
    ;; first analyze 3 parts.
    (let ((pproc (amb-analyze (if-predicate exp)))
          (cproc (amb-analyze (if-consequent exp)))
          (aproc (amb-analyze (if-alternative exp))))
      (lambda (env succeed fail)
        ;; evaluate predicate
        (pproc env
               ;; do evluation (on analyzed data)
               ;; obtain pred-value.
               (lambda (pred-value fail2)
                 ;; branching on previous result
                 (if (true? pred-value)
                     (cproc env succeed fail2)
                     (aproc env succeed fail2)))
               ;; revert if anything went wrong
               fail))))

  (define (test)
    (let* ((env (init-env))
          (tester (lambda (exp)
                    (amb-eval
                     exp
                     env
                     (lambda (val fail) val)
                     (lambda () 'failed)))))
      (do-test
       tester
       (list
        (mat `(if #t 10 20) 10)
        (mat `(if #f 11 22) 22))))
    'todo)

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
