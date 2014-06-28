;; an assertion to be added is a list
;; which begins with symbol "assert!"
(define (assertion-to-be-added? exp)
  (eq? (type exp) 'assert!))

(define (add-assertion-body exp)
  (car (contents exp)))

;; syntax of rules
;; rules are of the form:
;; '(rule <conclusion> [rule-body])
(define rule?
  (list-tagged-with 'rule))
(define conclusion cadr)
(define (rule-body rule)
  (if (null? (cddr rule))
      '(always-true)
      (caddr rule)))

(define (qeval-base-tests)
  ;; test about rules
  (do-test
   rule?
   (list
    (mat 'not-a-rule #f)
    (mat '(not-a-rule) #f)
    (mat '(rule conclusion) #t)
    (mat '(rule c (a b)) #t)))

  (do-test
   conclusion
   (list
    (mat '(rule concl) 'concl)
    (mat '(rule c (a b)) 'c)))

  (do-test
   rule-body
   (list
    (mat '(rule concl) '(always-true))
    (mat '(rule c (a b)) '(a b))))
  'ok)

(if *qeval-tests*
    (qeval-base-tests)
    'ok)

;; Local variables:
;; proc-entry: "./qeval.scm"
;; End:
