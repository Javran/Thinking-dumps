;;; working tests for ec-plus
(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "simu.scm")
(load "compiler.scm")
(load "ec-plus.scm")

(load "ec-tests.scm")
(for-each
 (test-evaluator machine-eval)
 test-exps)
(newline)

;; TODO: tests for argument ordering
(compile-and-go
 '(begin
    (define (fib n)
      (if (<= n 1)
          n
          (+ (fib (- n 1))
             (fib (- n 2)))))
    (begin
      (define x 1)
      (let ((a (begin
                 (set! x (+ x 10))
                 x))
            (b (begin
                 (set! x (* x 2))
                 x)))
        (cons a b)))))
