; `cond` is a list with the first element
;   being symbol `cond`
(define (cond? exp)
  (tagged-list? exp 'cond))

; there are clauses following
;   the symbol `cond`
(define cond-clauses cdr)

; for each clause, there are two parts:
(define cond-predicate car)
(define cond-actions cdr)

; an else clause is a clause whose predicate
;   part is a symbol `else`
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (last-exp? seq)
  ; `seq` must not be empty
  (null? (cdr seq)))
(define rest-exps cdr)
(define first-exp car)

; implement `cond` as syntactic sugar for `if` form.
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

; convert a sequence of actions to an expression
(define (sequence->exp seq)
  (cond ((null? seq)
          ; nothing to do if the sequence is empty
          seq)
        ((last-exp? seq)
          ; if there is only one element left,
          ;   this `seq` should be equivalent to
          ;   that element.
          (first-exp seq))
        (else
          ; elsewise, make it a `begin` form.
          (make-begin seq))))

; attach a `begin`
;   in front of the seq
(define (make-begin seq)
  (cons 'begin seq))

(define (expand-clauses clauses)
  (if (null? clauses)
    ; no case is given
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

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))
