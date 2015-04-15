(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "simu.scm")
(load "compiler.scm")
(load "ec-plus.scm")

;; based on exercise 5.48
;; * "compile" and "assemble" are combined together into "magic-compile"
;;   (this part is actually just reusing the implementation in ex 5.48)
;;   since we have to run "assemble" after "compile",
;;   I think we can just keep it simple by merging them into one single step.
;; * since everything will now be compiled, we can get rid of
;;   many code we have in the explicit evaluator
;; * as an addition point, we only need to deal with compiled-procedure
;;   if all we do is a "read-compile-execute-print" loop,
;;   because we don't need to interpret anything.

(load "exercise_5_48_prim.scm")
(load "exercise_5_48_machine.scm")
(load "exercise_5_49_compiler.scm")
(load "exercise_5_49_eval.scm")

(load "ec-tests.scm")

(for-each
 (test-evaluator machine-eval)
 test-exps) (newline)

(for-each
 (test-evaluator compile-and-run-with-env)
 test-exps) (newline)

(compile-and-go ''())
