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


(define impl2
  (let ()
    ; implementation 2: another alist 
    ;   use alist but every represetation holds the entire binding list 

    ; del-assv: Key x AList -> AList
    ; usage: remove key `obj` from `alist`
    (define (del-assv obj alist)
      (cond ((null? alist) '())
            ((eqv? (caar alist) obj)
              (cdr alist))
            (else
              (cons (car alist)
                    (del-assv obj (cdr alist))))))

    ; assv: Key x AList -> #f or (Key, Value)
    (define (assv obj alist)
      (cond ((null? alist) #f)
            ((eqv? (caar alist) obj)
              (car alist))
            (else
              (assv obj (cdr alist)))))

    ; empty-env: () -> Env
    (define (empty-env)
      '())

    ; extend-env: Var x SchemeVal X Env -> Env
    (define (extend-env key val env)
      (cons (cons key val)
            (del-assv key env)))

    ; apply-env: Env x Var -> SchemeVal
    (define (apply-env env var)
      (let ((result (assv var env)))
        (if result
          (cdr result)
          (report-no-binding-found var))))

    (list empty-env extend-env apply-env)))

(define impl3
  (let ()
    ; implementation 3: keep keys and values separately

    ; empty-env: () -> Env
    (define (empty-env)
      (cons '() '()))

    ; extend-env: Var x SchemeVal X Env -> Env
    (define (extend-env key val env)
      (let ((keys (car env))
            (vals (cdr env)))
        (cons (cons key keys)
              (cons val vals))))

    ; apply-env: Env x Var -> SchemeVal
    (define (apply-env env var)
      (let ((result (filter 
                      (lambda (x)
                        (eqv? var (car x)))
                      (map cons (car env) (cdr env)))))
        (if (null? result)
          (report-no-binding-found var)
          (cdar result))))

    (list empty-env extend-env apply-env)))

(apply do-env-test impl1)
(apply do-env-test impl2)
(apply do-env-test impl3)
