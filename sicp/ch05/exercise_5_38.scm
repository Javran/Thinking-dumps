(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "compiler.scm")
(load "simu.scm")
(load "simu_compiler_patch.scm")

;; warning: in this patch, the user will lose the ability
;; re-defining procedures which are recognized as open-code
;; primitives (as which has been indicated in the book's footnote)

;; for testing program, use "./exercise_5_38_tests.scm"
(define *ex-5.38-tests* #f)

(define factorial-code
  `(define (factorial n)
     (if (= n 1)
         1
         (* (factorial (- n 1)) n))))
;; compile using the original version
(define compiled-before
  (compile-and-check factorial-code))

(load "exercise_5_38_compiler.scm")

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
