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
(define (test-cmp equal1?)
  (lambda (result expected)
    (if (eq? expected 'failure)
        ;; a symbol "failure" means a failure on computation
        (not result)
        ;; otherwise, use equal1? to compare
        (equal1? (from-just result)
                 expected))))
