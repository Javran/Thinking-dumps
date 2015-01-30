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
  (lambda (exp env)
    ;; TODO: fill in env?
    (compile-and-run exp)))
 test-exps)

(end-script)
