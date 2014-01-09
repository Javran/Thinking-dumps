#lang eopl

(require "./common.rkt")

; # interfaces:
; ## constructors
; * var-exp
; * lambda-exp
; * app-exp
; ## predicates
; * var-exp?
; * lambda-exp?
; * app-exp?
; ## extractors
; * var-exp->var
; * lambda-exp->bound-var
; * lambda-exp->body
; * app-exp->rator
; * app-exp->rand

; ==== constructors ====

; var-exp: Var -> LcExp
(define var-exp identity)

; lambda-exp: Var x LcExp -> LcExp
(define (lambda-exp var e)
  (list
    'lambda
    (list (var-exp var))
    e))

; app-exp: LcExp x LcExp -> LcExp
(define (app-exp operator-e operand-e)
  (list
    operator-e
    operand-e))

; ==== predicates ====

; for simplicity, I assume the argument
;   is guaranteed to be constructed properly.
;   so I ignore all error handling
;   regarding ill-formed values

; var-exp?: LcExp -> Bool
(define var-exp? symbol?)

; lambda-exp?: LcExp -> Bool
(define (lambda-exp? e)
  (and (list? e)
       (eq? (car e) 'lambda)))

; app-exp?: LcExp -> Bool
(define (app-exp? e)
  (and (list? e)
       (list? (car e))))

; ==== extractors ====
; var-exp->var: LcExp -> Var
(define var-exp->var identity)

; lambda-exp->bound-var: LcExp -> Var
(define (lambda-exp->bound-var e)
  ; 3. extract using `var-exp->var`
  (var-exp->var
    ; 2. fetch the first element
    (car 
      ; 1. fetch the second element from the structure
      (cadr e))))

; lambda-exp->body: LcExp -> LcExp
(define lambda-exp->body caddr)

; app-exp->rator: LcExp -> LcExp
; usage: fetch the (ope)rator from an application
(define app-exp->rator car)

; app-exp->rand: LcExp -> LcExp
; usage: fetch the (ope)rand from an application
(define app-exp->rand cadr)

; test predicates
(let* ((v-e (var-exp 'x))
       (l-e (lambda-exp 'x v-e))
       (a-e (app-exp l-e l-e))
       (exprs (list v-e l-e a-e)))
  (assert
    (equal?
      (list #t #f #f) (map var-exp? exprs)))
  (assert
    (equal?
      (list #f #t #f) (map lambda-exp? exprs)))
  (assert
    (equal?
      (list #f #f #t) (map app-exp? exprs)))
  (out "predicates: assertion test passed.")
  'done)

; test extractors
(let* (; l-e = \ x -> \y -> x y
       (l-e (lambda-exp
              'x
              (lambda-exp
                'y
                (app-exp
                  (var-exp 'x)
                  (var-exp 'y))))))
  (define the-x
    (lambda-exp->bound-var l-e))
  (assert (eq? the-x 'x))
  (define the-y
    (lambda-exp->bound-var
      (lambda-exp->body l-e)))
  (assert (eq? the-y 'y))
  (define the-app
    (lambda-exp->body
      (lambda-exp->body l-e)))
  (define the-app-x
    (var-exp->var
      (app-exp->rator the-app)))
  (assert (eq? the-app-x 'x))
  (define the-app-y
    (var-exp->var
      (app-exp->rand the-app)))
  (assert (eq? the-app-y 'y))
  (out "extractors: assertion test passed.")
  'done)
