(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "compiler.scm")
(load "simu.scm")
(load "simu_compiler_patch.scm")

(load "exercise_5_42_compiler.scm")

;; TODO: see if it is applicable to apply "get-global-environment"
;; TODO: figure out what happens when there is a "define" form
;; inside a lambda expression
;; hope this does not screw up lexical addressing

(load "ec-tests.scm")
(load "exercise_5_23_tests.scm")

(for-each
 (test-evaluator
  compile-and-run-with-env)
 test-exps)

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
