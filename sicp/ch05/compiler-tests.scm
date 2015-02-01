(load "../common/utils.scm")
(load "../common/test-utils.scm")

;; compiler tests
(load "./compiler.scm")

;; borrow test framework from ec-tests
(load "./ec-tests.scm")
;; borrow testcases from ex 5.23
;; for testing basic expresssion & derived expressions
(load "./exercise_5_23_tests.scm")

(compiler-insn-seq-tests)
(newline)

(for-each
 (test-evaluator
  compile-and-run-with-env)
 test-exps)

(end-script)
