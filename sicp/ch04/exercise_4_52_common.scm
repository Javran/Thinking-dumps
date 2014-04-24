
(define (install-amb-if-fail)

  ;; (if-fail <try-body> <fallback value>)

  (define try-body cadr)
  (define fallback-val caddr)

  ;; "if-fail" as derived expression
  (define (analyze-if-fail-derived exp)
    ;; we need to provide a fallback if some expression have failed,
    ;; to do the trick, we observe that in "(amb <exp1> <exp2>)"
    ;; if "exp1" has failed, the second one will be attempted
    ;; therefore we can put "<try-body>" before "<fallback-val>"
    ;; in an "amb" special form.
    (amb-analyze `(amb ,(try-body exp)
                       ,(fallback-val exp))))

  (define (analyze-if-fail exp)
    (let ((try-body-a (amb-analyze (try-body exp)))
          (fallback-val-a (amb-analyze (fallback-val exp))))
      ;; will be called with env + succ cont + fail cont
      (lambda (env succeed fail)
        (try-body-a
         env
         succeed
         (lambda ()
           ;; when failed, evaluate fallback value
           (fallback-val-a
            env
            succeed
            ;; if fallback still fail, resort on the "fail" provided
            ;; from outside
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
     ;; the following two analyzers work equally well.
     analyze-if-fail
     ;; analyze-if-fail-derived
     test))

  (ahandler-register! handler)
  'ok)
