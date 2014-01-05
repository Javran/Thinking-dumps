#lang eopl

(require "./common.rkt")

(require "./ex-2.6-test.rkt")

(define (report-no-binding-found v)
  (eopl:error
    'apply-env
    "No binding for ~s"
    v))

(define impl1
  (let ()
    ; implementation 1: use procedures
    
    ; empty-env: () -> Env
    (define (empty-env)
      (lambda (v)
        ; when called with any variable,
        ;   raise an error
        (report-no-binding-found v)))

    ; extend-env: Var x SchemeVal X Env -> Env
    (define (extend-env key val env)
      (lambda (v)
        ; when called with a variable
        (if (eqv? v key)
          val
          (env v))))

    ; apply-env: Env x Var -> SchemeVal
    (define (apply-env env var)
      (env var))

    (list empty-env extend-env apply-env)))

(apply do-env-test impl1)
