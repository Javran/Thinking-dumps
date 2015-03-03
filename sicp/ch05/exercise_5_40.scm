(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "compiler.scm")
(load "simu.scm")
(load "simu_compiler_patch.scm")

(load "exercise_5_40_compiler.scm")

(load "ec-tests.scm")
(load "exercise_5_23_tests.scm")

;; Testcases here are for merely sanity checks.
;; We might want some tests to see if "ctenv" gets extended properly,
;; but the problem is that the compile-time environment - as its name suggests -
;; is available only at compile time, to see it in action,
;; it is more practical to write test cases in modules that uses the functionality
;; of this one.
;; Since "exercise_5_42.scm" is based on this module,
;; as long as that module does not go wrong,
;; we pretend the implementation here is good.
(for-each
 (test-evaluator
  compile-and-run-with-env)
 test-exps)

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
