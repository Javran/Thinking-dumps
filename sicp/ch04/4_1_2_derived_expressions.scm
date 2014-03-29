(load "../common/utils.scm")
(load "../common/test-utils.scm")

(define (cond? exp)
  (tagged-list? exp 'cond))

(define cond-predicate car)
(define cond-actions cdr)

(define cond-clauses cdr)
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))
(define (expand-clauses clauses)
  (if (null? clauses)
    'false
    (let ((first (car clauses))
          (rest  (cdr clauses)))
      (if (cond-else-clause? first)
        (if (null? rest)
          ; else part ... convert the seq to exp
          (sequence->exp (cond-actions first))
          (error "ELSE clause isn't last: COND->IF"
                 clauses))
        (make-if (cond-predicate first)
                 (sequence->exp (cond-actions first))
                 (expand-clauses rest))))))


; missing definitions:
; * define-variable!
; * set-variable-value!
; * true?
; * lookup-variable-value
; * make-procedure
; * primitive-procedure?
; * apply-primitive-procedure
; * compound-procedure?
; * procedure-body
; * extend-environment
; * procedure-parameters
; * procedure-environment

(end-script)
