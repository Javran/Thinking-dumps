#lang eopl

(require "./common.rkt")

(require "./ex-2.6-test.rkt")

; interfaces:
; * empty-env: () -> Env
; * extend-env: Var x SchemeVal x Env -> Env
; * extend-env*: [Var] x [SchemeVal] x Env -> Env
; * apply-env: Env x Var -> SchemeVal

(define (report-no-binding-found v)
  (eopl:error
    'apply-env
    "No binding for ~s"
    v))

; empty-env: () -> Env
(define (empty-env)
  '())

; extend-env: Var x SchemeVal x Env -> Env
(define (extend-env var val env)
  (cons (cons (list var)
              (list val))
        env))

; extend-env*: [Var] x [SchemeVal] x Env -> Env
(define (extend-env* vars vals env)
  (cons (cons vars vals)
        env))

; apply-env: Env x Var -> SchemeVal
(define (apply-env env search-var)
  (if (null? env)
    (report-no-binding-found search-var)
    ; else
    (let* ((current-env (car env))
           (the-pair
            (memf
              (lambda (pair)
                ; 2. find the variable name in pair list
                (eq? (car pair) search-var))
              ; 1. zip variables and values together
              (map cons (car current-env) (cdr current-env)))))
      (if the-pair
        (cdar the-pair)
        (apply-env (cdr env) search-var)))))

(do-env-test empty-env extend-env apply-env)

(define env
  (extend-env*
    '(a b c d)
    '(1 2 3 4)
    (extend-env*
      '(e f g h)
      '(5 6 7 8)
      (extend-env*
        '(i j)
        '(9 0)
        (empty-env)))))

(out (apply-env env 'a)
     (apply-env env 'f)
     (apply-env env 'i))
; 1, 6, 9
