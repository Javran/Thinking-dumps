#lang eopl

(require "./common.rkt")

(require "./ex-2.15-test.rkt")

(define impl1
  (let ()
    ; implementation #1: use list

    ; ==== constructors ====

    ; var-exp: Var -> LcExp
    (define (var-exp v)
      (list 'var v))

    ; lambda-exp: Var x LcExp -> LcExp
    (define (lambda-exp v e)
      (list 'lambda v e))

    ; app-exp: LcExp x LcExp -> LcExp
    (define (app-exp e1 e2)
      (list 'app e1 e2))

    ; ==== predicates ====

    ; var-exp?: LcExp -> Bool
    (define (var-exp? e)
      (eq? 'var (car e)))

    ; lambda-exp?: LcExp -> Bool
    (define (lambda-exp? e)
      (eq? 'lambda (car e)))

    ; app-exp?: LcExp -> Bool
    (define (app-exp? e)
      (eq? 'app (car e)))

    ; ==== extractors ====

    ; var-exp->var: LcExp -> Var
    (define var-exp->var cadr)

    ; lambda-exp->bound-var: LcExp -> Var
    (define lambda-exp->bound-var cadr)

    ; lambda-exp->body: LcExp -> LcExp
    (define lambda-exp->body caddr)

    ; app-exp->rator: LcExp -> LcExp
    (define app-exp->rator cadr)

    ; app-exp->rand: LcExp -> LcExp
    (define app-exp->rand caddr)

    (list
      var-exp
      lambda-exp
      app-exp
      ;
      var-exp?
      lambda-exp?
      app-exp?
      ;
      var-exp->var
      lambda-exp->bound-var
      lambda-exp->body
      app-exp->rator
      app-exp->rand)))

(apply do-lc-exp-test impl1)

#|
(define impl2
  ; implementation #2: todo...

  ; ==== constructors ====

  ; var-exp: Var -> LcExp

  ; lambda-exp: Var x LcExp -> LcExp

  ; app-exp: LcExp x LcExp -> LcExp

  ; ==== predicates ====

  ; var-exp?: LcExp -> Bool

  ; lambda-exp?: LcExp -> Bool

  ; app-exp?: LcExp -> Bool

  ; ==== extractors ====

  ; var-exp->var: LcExp -> Var

  ; lambda-exp->bound-var: LcExp -> Var

  ; lambda-exp->body: LcExp -> LcExp

  ; app-exp->rator: LcExp -> LcExp

  ; app-exp->rand: LcExp -> LcExp

  (list
    var-exp
    lambda-exp
    app-exp
    ;
    var-exp?
    lambda-exp?
    app-exp?
    ;
    var-exp->var
    lambda-exp->bound-var
    lambda-exp->body
    app-exp->rator
    app-exp->rand))
|#
