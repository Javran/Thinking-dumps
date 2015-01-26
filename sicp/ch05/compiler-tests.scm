(load "../common/utils.scm")
(load "../common/test-utils.scm")

;; compiler tests
(load "./compiler.scm")

(compiler-insn-seq-tests)
(newline)

(compile-and-run
 (compile '(begin
             (define (factorial n)
               (if (= n 1)
                   1
                   (* (factorial (- n 1)) n)))
             (factorial 5))
          'val 'next))

(end-script)
