(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "simu.scm")
(load "compiler.scm")
(load "ec-plus.scm")

(load "exercise_5_47_compiler.scm")

(load "exercise_5_49_eval.scm")
(load "exercise_5_48_prim.scm")
(load "exercise_5_48_machine.scm")

(load "ec-tests.scm")
;; since now we don't have to evaluate
;; at run time, it's not necessary for "evaluator-insns"
;; to handle evaluations
;; and that might break "machine-eval"
;; TODO: either don't call our new RCEPL instructions the same name
;; or we abandon this part of test altogether
#|
(for-each
 (test-evaluator machine-eval)
 test-exps) (newline)
|#

(for-each
 (test-evaluator compile-and-run-with-env)
 test-exps) (newline)

(compile-and-go ''())

;; TODO: exercise 5.49. will be based on exercise 5.48
;; * "compile" and "assemble" can be merged into one operation
;;   since we have to run "assemble" after "compile" (as we did in ex 5.48)
;; * since everything will now be compiled, I think we can get rid of
;;   many code we have in the explicit evaluator
;; * as an addition point, we only need to deal with compiled-procedure
;;   if all we do is a "read-compile-execute-print" loop,
;;   because we don't need to interpret anything!
