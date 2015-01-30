(load "../common/utils.scm")
(load "../common/test-utils.scm")

;; compiler tests
(load "./compiler.scm")

;; borrow tests from ec-tests
(load "./ec-tests.scm")

(compiler-insn-seq-tests)
(newline)

(for-each
 (test-evaluator
  compile-and-run-with-env)
 test-exps)

(end-script)
