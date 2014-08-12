(define (list-tagged-with tag)
  (lambda (l)
    (and
      (list? l)
      (non-empty? l)
      (eq? (car l) tag))))

(define (tagged-list? exp tag)
  ((list-tagged-with tag) exp))

;; accessors for "assign"
;; (assign <reg> @<value-exp> ..)
(define assign-reg-name cadr)
(define assign-value-exp cddr)

;; accessors for "test"
;; (test @<condition>)
(define test-condition cdr)

;; accessors for "branch"
;; (branch <destination>)
(define branch-dest cadr)

;; accessors for "goto"
;; (goto <destionation>)
(define goto-dest cadr)

;; accessors for "perform"
;; (perform @<inst>)
(define (perform-action inst) (cdr inst))
