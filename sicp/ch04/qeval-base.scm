;; utils
(define (list-tagged-with tag)
  (lambda (l)
    (and
      (list? l)
      (non-empty? l)
      (eq? (car l) tag))))

;; a special form is identified by the "car" part
;; of it, which is called "type".
;; and rest of the data is the "contents"
(define (type exp)
  (if (pair? exp)
      (car exp)
      (error "Unknown expression TYPE"
             exp)))

(define (contents exp)
  (if (pair? exp)
      (cdr exp)
      (error "Unknown expression CONTENTS"
             exp)))

;; an assertion to be added is a list
;; which begins with symbol "assert!"
(define (assertion-to-be-added? exp)
  (eq? (type exp) 'assert!))

(define (add-assertion-body exp)
  (car (contents exp)))

;; conjunctions
(define empty-conjunction? null?)
(define first-conjunct car)
(define rest-conjuncts cdr)

;; disjunctions
(define empty-disjunction? null?)
(define first-disjunct car)
(define rest-disjuncts cdr)

;; "not"
(define negated-query car)

;; lisp-value
(define predicate car)
(define args cdr)

;; syntax of rules
(define rule?
  (list-tagged-with 'rule))
(define conclusion cadr)
(define (rule-body rule)
  (if (null? (cddr rule))
      '(always-true)
      (caddr rule)))

(define var?
  (list-tagged-with '?))
(define constant-symbol? symbol?)

(define rule-counter 0)
(define (new-rule-application-id)
  (set! rule-counter (add1 rule-counter))
  rule-counter)

(define (make-new-variable var rule-application-id)
  (cons '? (cons rule-application-id (cdr vanr))))

(define (contract-question-mark variable)
  (string->symbol
   (string-append
    "?"
    (if (number? (cadr variable))
        (string-append (symbol->string (caddr variable))
                       "-"
                       (number->string (cadr variable)))
        (symbol->string (cadr variable))))))
