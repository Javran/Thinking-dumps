(load "../common/utils.scm")
(load "../common/test-utils.scm")

;; compiler tests
(load "compiler.scm")

(load "simu.scm")
(load "simu_compiler_patch.scm")

;; borrow test framework from ec-tests
(load "ec-tests.scm")
;; borrow testcases from ex 5.23
;; for testing basic expresssion & derived expressions
(load "exercise_5_23_tests.scm")

(define test-exps-and-or-tests
  `(;; no arg
    (and)
    ;; one arg, with side effect
    (begin
      (define x 10)
      (and (begin
             (set! x (+ x 1))
             x)))
    (begin
      (define x 10)
      (and (begin
             (set! x (+ x 10))
             x)
           (begin
             (set! x (* x 20))
             x)
           x))
    (and 1 2 #f no-such-variable)

    (or)
    (begin
      (define x 10)
      (or (begin
            (set! x (+ x 1))
            x)))
    (begin
      (define x 10)
      (or (begin
             (set! x (+ x 10))
             #f)
           (begin
             (set! x (* x 20))
             #f)
           x))
    (or #t no-such-variable)
    ))

(set! test-exps
      (append test-exps test-exps-and-or-tests))

(for-each
 (test-evaluator
  ;; compile and evalute the code
  ;; and compare it with the result of evaluating
  ;; the code using native evaluator from scheme
  compile-and-run-with-env)
 test-exps)

(end-script)
