(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./compiler.scm")
(load "./simu.scm")
(load "./simu_compiler_patch.scm")

(define factorial-code
  `(define (factorial n)
     (if (= n 1)
         1
         (* (factorial (- n 1)) n))))
;; compile using the original version
(define compiled-before
  (compile-and-check factorial-code))

(load "./exercise_5_38_compiler.scm")

;; run tests on the modified compiler
(load "./ec-tests.scm")
(load "./exercise_5_23_tests.scm")
(for-each
 (test-evaluator
  compile-and-run-with-env)
 test-exps)
(newline)

;; compile again using new compiler
(define compiled-after
  (compile-and-check factorial-code))

(out ";; ==== original version:")
(print-instruction-sequence compiled-before)
(newline)
(out ";; ==== with open-code primitives:")
(print-instruction-sequence compiled-after)

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
