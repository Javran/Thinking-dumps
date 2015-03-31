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
(for-each
 (test-evaluator compile-and-run-with-env)
 test-exps)
(newline)

(define arg-eval-ord-test-expr
  `(begin
     (define x 1)
     (let ((a (begin
                (set! x (+ x 10))
                x))
           (b (begin
                (set! x (* x 2))
                x)))
       (cons a b))))

(assert
 (equal? (machine-eval
          arg-eval-ord-test-expr
          (init-env))
         '(11 . 22))
 "assertion on argument evaluation ordering failed for the evaluator")

(assert
 (equal? (compile-and-run-with-env
          arg-eval-ord-test-expr
          (init-env))
         '(11 . 22))
 "assertion on argument evaluation ordering failed for the compiler")

;; simply run the REPL for testing
(compile-and-go ''())

