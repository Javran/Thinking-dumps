(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./amb-eval.scm")

;; still not sure if my solution is the true intention of
;; exercise 4.52, but here my solution does match the output
;; given in the exercise.
;; The weird thing is: in the exercise, if the list does not
;; contain any odd numbers, "all-odd" is print, and if the list
;; has one even number, what will happen after that even number
;; get printed? "all-odd" sounds not good literally.

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

(install-amb-if-fail)

(run-all-slot-tests)

(define (test-prog l)
  `(if-fail (let ((x (amb ,@l)))
              (define (require x)
                (if x 'pass (amb)))
              (require (even? x))
              x)
            'all-odd))

(out (amb-eval-all (test-prog '(1 3 5)) (init-env)))
(out (amb-eval-all (test-prog '(1 3 5 8)) (init-env)))

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
