(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "simu.scm")
(load "compiler.scm")
(load "ec-plus.scm")

(load "exercise_5_47_eval.scm")
(load "exercise_5_47_compiler.scm")

(load "ec-tests.scm")
(for-each
 (test-evaluator compile-and-run-with-env)
 test-exps)
(newline)

(compile-and-go
 '(begin
    (define (map f xs)
      (if (null? xs)
          '()
          (cons (f (car xs))
                (map f (cdr xs)))))))
