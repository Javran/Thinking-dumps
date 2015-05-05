;; all tests in the original metacircular evaluator is abandoned here.
;; for simplicity, we will instead just test the behavior of our evaluator:
;; we reuse tests and the tester in "ec-tests.scm", if the metacircular evaluator
;; agrees with our evaluator in the implementing language, then it is fine.

(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "ec-tests.scm")

(load "metacircular-evaluator.scm")

;; assume  "apply-prim"
;; does primitive application
(define apply-prim apply)

(load "metacircular-evaluator-tests-extras.scm")

(set! test-exps
      (append test-exps
              test-exps-metacircular))

(my-eval-select-approach 'interpret)
(for-each
 (test-evaluator my-eval)
 test-exps) (newline)

(my-eval-select-approach 'analyze)
(for-each
 (test-evaluator my-eval)
 test-exps) (newline)
