;;; explicit control evaluator with compiled-code supports

;; there is an inconsistency between our evaluator and compiler:
;; when a function is applied with arguments,
;; the evaluator evaluates arguments from left to right while
;; the code produces by the compiler evaluates arguments from right to left.
;; this might lead to many confusions
;; so the first problem to be solved is to make this consistent:
;; we modify the compiler so that it produces code that evaluates
;; the argument from left to right.

;; to verify the order of argument evaluation,
;; we can use the following expression:

#|
(begin
  (define x 1)
  (let ((a (begin
             (set! x (+ x 10))
             x))
        (b (begin
             (set! x (* x 2))
             x)))
    (cons a b)))
|#

;; the result will be:
;; * (11 . 22) if arguments are evaluated from left to right
;; * (12 . 2)  if arguments are evaluated from right to left

(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "simu.scm")
(load "simu_ec_patch.scm")
(load "exercise_5_23_common.scm")
(load "ec-plus-eval.scm")

(load "compiler.scm")

(load "ec-tests.scm")
(for-each
 (test-evaluator machine-eval)
 test-exps)
(newline)

;; TODO: I think the primitive operations used in evaluator
;; should be a superset of those used in a compiler,
;; maybe we can just use those primitives in the evaluator

;; TODO: we need some document about how could compiled code
;; being inserted at run time, doesn't this require the set
;; of instruction sequence being changed somehow?

(out
 (if (check-labels evaluator-insns)
     "no problem with the label checker"
     "some missing labels found"))
