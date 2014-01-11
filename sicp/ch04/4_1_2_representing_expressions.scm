(load "../common/utils.scm")
(load "../common/test-utils.scm")

(define (self-evaluating? exp)
  ; refactored
  ; p.s.: what about characters?
  (or (number? exp)
      (string? exp)))

(define variable? symbol?)

; quotations are of the form: `(quote <text-of-quotation>)`
(define (quoted? exp)
  ; what is `tagged-list?` ?
  (tagged-list? exp 'quote))
(define (text-of-quotation exp)
  (cadr exp))

(define (tagged-list? exp tag)
  ; lists beginning with a designated symbol
  ; refactored
  (and (non-empty? exp)
       (eq? (car exp) tag)))

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
    (caddr exp)))
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

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (if? exp)
  (tagged-list? exp 'if))
(define if-predicate cadr)
(define if-consequent caddr)
(define (if-alternative exp)
  ; allowing the else part not filled.
  (if (not (null? (cdddr exp)))
    (caddr exp)
    'false))
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (begin? exp)
  (tagged-list? exp 'begin))

(define begin-actions cdr)
(define (last-exp? seq)
  ; `seq` must not be empty
  (null? (cdr seq)))
(define first-exp car)
(define rest-exps cdr)

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))

(define application? non-empty?)
(define operator car)
(define operands cdr)
(define no-operands? null?)
(define first-operand car)
(define rest-operands cdr)

; missing definitions:
; * define-variable!
; * set-variable-value!
; * true?
; * lookup-variable-value
; * make-procedure
; * cond?
; * cond->if
; * primitive-procedure?
; * apply-primitive-procedure
; * compound-procedure?
; * procedure-body
; * extend-environment
; * procedure-parameters
; * procedure-environment

(end-script)
