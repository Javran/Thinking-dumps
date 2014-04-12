(load "../common/utils.scm")
(load "../common/test-utils.scm")

;; using the framework of my-eval.
;; disabling all tests for compatibility.
(define *my-eval-do-test* #f)

(load "./my-eval-data-directed.scm")
(load "./my-eval-env.scm")
(load "./my-eval-utils.scm")

(load "./my-eval-apply.scm")
(load "./my-eval-init-env.scm")

(load "./amb-eval-test.scm")

(load "./amb-eval-analyze.scm")

(load "./amb-eval-e-lambda.scm")
(load "./amb-eval-e-if.scm")
(load "./amb-eval-e-begin.scm")
(load "./amb-eval-e-define.scm")
(load "./amb-eval-e-set.scm")

(install-amb-if)
(install-amb-lambda)
(install-amb-begin)
(install-amb-define)

(run-all-slot-tests)

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
