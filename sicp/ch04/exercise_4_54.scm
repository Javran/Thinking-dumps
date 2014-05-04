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
                 ;; the negation is not necessary
                 ;; the following code is my answer to ex 4.54
                 ;; but we can do sligthly better.
                 ;; (if (not pred-value)
                 ;;    (fail2)
                 ;;    (succeed 'ok fail2)))
                 (if pred-value
                     (succeed 'ok fail2)
                     (fail2)))
               fail))))

  (define (test)
    (let ((env (init-env)))
      (do-test
       amb-eval-all
       (list
        (mat `(let ((x (amb 1 2 3)))
                (require (even? x))
                x)
             env '(2))
        (mat `(let ((x (amb 1 3 5)))
                (require (even? x))
                x)
             env '())
        (mat `(let ((x (amb 1 2 3 4))
                    (y (amb 5 6 7 8)))
                (require (= (+ x y) 8))
                (cons x y))
             env '((1 . 7) (2 . 6) (3 . 5)))
        )))
    'ok)

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
