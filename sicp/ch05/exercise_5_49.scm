(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "simu.scm")
(load "compiler.scm")
(load "ec-plus.scm")

(load "exercise_5_47_compiler.scm")

(load "exercise_5_48_eval.scm")
(load "exercise_5_48_prim.scm")
(load "exercise_5_48_machine.scm")

(load "ec-tests.scm")
(for-each
 (test-evaluator machine-eval)
 test-exps) (newline)
(for-each
 (test-evaluator compile-and-run-with-env)
 test-exps) (newline)

;; TODO: exercise 5.49. will be based on exercise 5.48
