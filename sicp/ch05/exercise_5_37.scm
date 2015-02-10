(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./compiler.scm")
(load "./simu.scm")
(load "./simu_compiler_patch.scm")

(load "./exercise_5_37_compiler.scm")

(load "./ec-tests.scm")
(load "./exercise_5_23_tests.scm")
(for-each
 (test-evaluator
  compile-and-run-with-env)
 test-exps)
(newline)

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
