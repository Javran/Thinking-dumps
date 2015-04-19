;; tests in the original metacircular should be moved to here
;; and instead of having more fine-grained tests
;; we will just test the behavior: if our metacircular evaluator
;; gives the same result as the one given by the evaluator in our implementing
;; language, then it is fine.

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
