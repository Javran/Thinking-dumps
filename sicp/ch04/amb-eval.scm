;; despite that the whole system is working,
;; we need to automatically fetch all the solutions.
;; I don't have a good idea for now.

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
(load "./amb-eval-driver-loop.scm")

(load "./amb-eval-test.scm")

(load "./amb-eval-analyze.scm")

(load "./amb-eval-e-quote.scm")
(load "./amb-eval-e-lambda.scm")
(load "./amb-eval-e-if.scm")
(load "./amb-eval-e-begin.scm")
(load "./amb-eval-e-define.scm")
(load "./amb-eval-e-set.scm")
(load "./amb-eval-e-amb.scm")

(install-amb-quote)
(install-amb-lambda)
(install-amb-if)
(install-amb-begin)
(install-amb-define)
(install-amb-set!)
(install-amb-amb)

;; disable slot tests when being used as a module
;; user can still call this procedure explictly
(run-all-slot-tests)

;; it's recommended using `(amb-repl)` to start the driver loop

;; Local variables:
;; proc-entry: ""
;; End:
