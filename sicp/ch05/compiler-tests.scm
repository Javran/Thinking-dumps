(load "../common/utils.scm")
(load "../common/test-utils.scm")

;; compiler tests
(load "./compiler.scm")

(compiler-insn-seq-tests)
(newline)

(for-each out (statements
               (compile '((define (factorial n)
                            (if (= n 1)
                                1
                                (* (factorial (- n 1)) n))))
                        'val 'next)))
(end-script)
