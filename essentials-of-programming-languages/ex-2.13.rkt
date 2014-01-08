#lang eopl

(require "./common.rkt")

(define (report-no-binding-found v)
  (eopl:error
    'apply-env
    "No binding for ~s"
    v))

; empty-env: () -> Env
(define (empty-env)
  (list 
    ; apply-env
    (lambda (v)
      ; when called with any variable,
      ;   raise an error
      (report-no-binding-found v))
    ; empty-env?
    (lambda () #t)))

; extend-env: Var x SchemeVal x Env -> Env
(define (extend-env key val env)
  (list 
    ; apply-env
    (lambda (v)
      ; when called with a variable
      (if (eqv? v key)
        val
        (apply-env env v)))
    ; empty-env?
    (lambda () #f)))

; apply-env: Env x Var -> SchemeVal
(define (apply-env env var)
  ((car env) var))

; empty-env?: Env -> Bool
(define (empty-env? env)
  ((cadr env)))

(require "./ex-2.6-test.rkt")

(do-env-test empty-env extend-env apply-env)

(assert (empty-env? (empty-env)))
(assert (not (empty-env? (extend-env 'foo 'bar (empty-env)))))
(out "assertions passed.")

