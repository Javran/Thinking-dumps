(load "../common/utils.scm")
(load "../common/test-utils.scm")

;; compiler tests
(load "./compiler.scm")

(compiler-insn-seq-tests)

(for-each out (statements (compile '(+ 1 2 3) 'val 'next)))

(end-script)
