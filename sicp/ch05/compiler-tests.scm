(load "../common/utils.scm")
(load "../common/test-utils.scm")

;; compiler tests
(load "./compiler.scm")

(compiler-insn-seq-tests)
(newline)

(print-instruction-sequence
 (compile '((define (factorial n)
              (if (= n 1)
                  1
                  (* (factorial (- n 1)) n))))
          'val 'next))
(end-script)
