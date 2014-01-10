#lang eopl

(require "./common.rkt")

(define-datatype env-exp env-exp?
  (empty-env)
  (extend-env
    (var symbol?)
    (val (const #t))
    (env env-exp?)))

; test env-exp
(define e
  (extend-env
    'a 10 
    (extend-env 
      'b 20
      (empty-env))))

(define (env-exp->string env)
  (cases env-exp env
    (empty-env ()
      "(empty-env)")
    (extend-env (var val subenv)
      (format "~A => ~A, ~A" var val (env-exp->string subenv)))))

; has-binding?: EnvExp -> Var -> Bool
; usage: this `env` has binding?
(define (has-binding? env s)
  (cases env-exp env
    (empty-env ()
      #f)
    (extend-env (var val subenv) 
      (or (eq? s var)
          (has-binding? subenv s)))))

(out (env-exp->string e))

(out (has-binding? (empty-env) 'a)
     (has-binding? e 'a)
     (has-binding? e 'b)
     (has-binding? e 'c))
; #f, #t, #t, #f
