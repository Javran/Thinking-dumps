(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "compiler.scm")
(load "simu.scm")
(load "simu_compiler_patch.scm")

(load "exercise_5_44_compiler.scm")

(load "ec-tests.scm")
(load "exercise_5_23_tests.scm")
(load "exercise_5_43_tests.scm")

;; plan:
;; * tests

;; testing the compiler
(for-each
 (test-evaluator
  compile-and-run-with-env)
 test-exps)

(end-script)
