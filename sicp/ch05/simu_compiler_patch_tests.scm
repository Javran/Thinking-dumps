(load "../common/utils.scm")
(load "../common/test-utils.scm")

;; compiler tests
(load "./compiler.scm")

(load "./simu.scm")
(load "./simu_compiler_patch.scm")

;; borrow test framework from ec-tests
(load "./ec-tests.scm")
;; borrow testcases from ex 5.23
;; for testing basic expresssion & derived expressions
(load "./exercise_5_23_tests.scm")

(for-each
 (test-evaluator
  ;; compile and evalute the code
  ;; and compare it with the result of evaluating
  ;; the code using native evaluator from scheme
  compile-and-run-with-env)
 test-exps)

(end-script)
