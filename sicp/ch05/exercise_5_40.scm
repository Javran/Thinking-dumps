(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "compiler.scm")
(load "simu.scm")
(load "simu_compiler_patch.scm")

(load "exercise_5_40_compiler.scm")

(load "ec-tests.scm")
(load "exercise_5_23_tests.scm")

;; testcases here are for merely sanity checks.
;; TODO: for now "ctenv" are just passed around and
;; we really want to see if it works for "compile-lambda-body"
(for-each
 (test-evaluator
  compile-and-run-with-env)
 test-exps)

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
