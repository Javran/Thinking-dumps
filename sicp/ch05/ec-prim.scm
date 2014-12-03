;; primitives

;; helper
(define (list-tagged-with tag)
  (lambda (l)
    (and
      (list? l)
      (non-empty? l)
      (eq? (car l) tag))))

;; check if an expression is self-evaluating
;; i.e. (eval env exp) == exp
(define (self-evaluating? exp)
  (or (number? exp)
      (string? exp)
      (char? exp)
      (boolean? exp)
      (null? exp)))

;; if an given expression is a variable
(define variable? symbol?)

;; quoted expression
(define quoted?
  (list-tagged-with 'quote))

(define (assignment? exp)
  (tagged-list? exp 'set!))
(define assignment-variable cadr)
(define assignment-value caddr)

(define (definition? exp)
  (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
    ; form: (define <var> <value>)
    (cadr exp)
    ; `(cadr exp)` is a list
    (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
    ; first form
    (caddr exp)
    ; second form, need desugar
    (make-lambda
      (cdadr exp)   ; formal parameters
      (cddr exp)))) ; body

(define (lambda? exp)
  (tagged-list? exp 'lambda))
(define lambda-parameters cadr)
(define lambda-body cddr)

(define (if? exp)
  (tagged-list? exp 'if))
(define if-predicate cadr)
(define if-consequent caddr)
(define (if-alternative exp)
  ; allowing the else part to be empty
  (if (not (null? (cdddr exp)))
    (cadddr exp)
    #f))

(define (true? d)
  (if d #t #f))
(define (false? d)
  (if d #f #t))

;; (begin? 1)
;; (application? 1)
;; (lookup-variable-value 2)
;; (text-of-quotation 1)
;; (make-procedure 3)
;; (operands 1)
;; (operator 1)
;; (empty-arglist 0)
;; (no-operands? 1)
;; (first-operand 1)
;; (last-operand? 1)
;; (adjoin-arg 2)
;; (rest-operands 1)
;; (primitve-procedure? 1)
;; (compound-procedure? 1)
;; (apply-primitive-procedure 2)
;; (procedure-parameters 1)
;; (procedure-environment 1)
;; (extend-environment 3)
;; (procedure-body 1)
;; (begin-actions 1)
;; (first-exp 1)
;; (last-exp? 1)
;; (rest-exps 1)
;; (set-variable-value! 3)
;; (define-variable! 3)
