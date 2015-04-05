(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "simu.scm")
(load "compiler.scm")
(load "ec-plus.scm")

(load "exercise_5_47_eval.scm")
(load "exercise_5_47_compiler.scm")

(load "ec-tests.scm")
(for-each
 (test-evaluator machine-eval)
 test-exps) (newline)

;; some testcases might fail when the compilation
;; is not fully working, this is desired because
;; at compile time we cannot check the object and
;; make the final decision so we must take all the cases
;; into account, the compilation of compound procedure application
;; is one of these cases.
(for-each
 (test-evaluator compile-and-run-with-env)
 test-exps) (newline)

;; TODO: need to summarize 3 cases.
(compile-and-go
 '(begin
    (define (compose g f)
      (lambda (x)
        (g (f x))))
    ;; for triggering target == val && linkage == return
    ;; use: ((test-apply (lambda (x) x)) 1)
    (define (test-apply f)
      (lambda (x) (f x)))
    (define (identity x) x)
    ;; for triggering target == val && linkage /= return
    (define (map f xs)
      (if (null? xs)
          '()
          (cons (f (car xs))
                (map f (cdr xs)))))))
