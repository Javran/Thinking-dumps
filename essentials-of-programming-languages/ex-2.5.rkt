#lang eopl

(require "./common.rkt")

; interfaces:
; 
; constructors:
; * (empty-env)
; * (extend-env var v f)
;
; observers:
; * (apply-env f var)

; empty-env: () -> Env
(define (empty-env)
  '())

; extend-env: Var x SchemeVal x Env -> Env
(define (extend-env key val env)
  (cons (cons key val)
        env))

; apply-env: Env x Var -> SchemeVal
(define (apply-env env var)
  (if (null? env)
    (report-no-binding-found var)
    ; else, non-empty env
    (let ((current-k-v (car env)))
      (cond ((eqv? (car current-k-v) var)
              (cdr current-k-v))
            (else
              (apply-env (cdr env) var))))))

(define (report-no-binding-found var)
  (eopl:error
    'apply-env
    "No binding for ~s"
    var))

(define e
  (extend-env
    'x 1
    (extend-env
      'y 2
      (extend-env
        'x 3
        (extend-env
          'z 4
          (empty-env))))))

(out (apply-env e 'x)
     (apply-env e 'y)
     (apply-env e 'z))

(require "./ex-2.6-test.rkt")

(do-env-test
  empty-env
  extend-env
  apply-env)
