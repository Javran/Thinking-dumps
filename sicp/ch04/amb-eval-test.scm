;; amb-eval test facilities

(load "./my-eval-maybe.scm")

(define (test-on-success exp fail)
  (just exp))

(define (test-on-failure)
  #f)

;; make the call looks more like a regular eval function
;; the result will be wrap inside a `just`, or #f on failure
(define (test-eval exp env)
  (amb-eval exp env
            test-on-success
            test-on-failure))

;; assume the result is wrapped in a "just"
;; and when `expected` is "failure", that means a failure on computation
;; deal with the result comparison properly
(define (test-compare equal1?)
  (lambda (result expected)
    (if (eq? expected 'failure)
        ;; a symbol "failure" means a failure on computation
        (not result)
        ;; otherwise, use equal1? to compare
        (equal1? (from-just result)
                 expected))))

;; run test in a specified slot
(define (run-slot-test slot)
  (format #t "Testing slot: ~A " slot)
  (let ((handler (my-eval-get slot)))
    (if (ahandler? handler)
        (ahandler-run-test handler)
        (error "no such slot" slot))))

(define (test-result-print result)
  (if (eq? result 'ok)
      (out "Passed")
      (format #t "NotPassing: ~A~%" result)))

;; run tests on all registered handlers
(define (run-all-slot-tests)
  (for-each
   (compose test-result-print run-slot-test)
   (my-eval-get-all-slot-names)))
