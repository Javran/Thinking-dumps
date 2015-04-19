;; all tests in the original metacircular evaluator is abandoned here.
;; for simplicity, we will instead just test the behavior of our evaluator:
;; we reuse tests and the tester in "ec-tests.scm", if the metacircular evaluator
;; agrees with our evaluator in the implementing language, then it is fine.
(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "metacircular-evaluator.scm")

(load "ec-tests.scm")

(my-eval-select-approach 'interpret)
(for-each
 (test-evaluator my-eval)
 test-exps) (newline)

(my-eval-select-approach 'analyze)
(for-each
 (test-evaluator my-eval)
 test-exps) (newline)
