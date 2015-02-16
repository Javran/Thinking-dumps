(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./compiler.scm")
(load "./simu.scm")
(load "./simu_compiler_patch.scm")

(define *ex-5.38-test* #t)

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
(if *ex-5.38-test*
    (begin
      (for-each
       (test-evaluator
        compile-and-run-with-env)
       test-exps)
      (newline))
    'skipped)

;; compile again using new compiler
(define compiled-after
  (compile-and-check factorial-code))

(out ";; ==== original version:")
(print-instruction-sequence compiled-before)
(newline)
(out ";; ==== with open-code primitives:")
(print-instruction-sequence compiled-after)

;; TODO: "+" and "*" can accept arbitrary numbers
;; of operands because (number,+) and (number,*) are commutative monoids
;; it doesn't matter how they gets combined together.
;; rather than taking care of register handlings, here is my plan:
;; * enforce "spread-arguments" to deal with two-argument cases only
;; * do syntactic transformation before compilation
;;   e.g.
;;   (+ 1)         => 1
;;   (+ 1 2)       => (+ 1 2)
;;   (+ 1 2 3)     => (+ 1 (+ 2 3))
;;   (+ 1 2 3 4)   => (+ 1 (+ 2 (+ 3 4)))
;;   ...
;; * however, the side effects incurred by evaluating arguments
;;   is not guaranteed to happen in a certain order, since
;;   different compilers can make different decisions about
;;   the argument evaluation order. here, in order to keep
;;   consistent with the original implementation,
;;   we choose to "fold" from right to left
;;   therefore arguements will still be evaluated from right to left

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
