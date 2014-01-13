#lang eopl

(require "../common.rkt")

; empty-env: () -> Env
(define (empty-env)
  (list 'empty-env))

; extend-env: Var x SchemeVal X Env -> Env
(define (extend-env var val env)
  (list 'extend-env var val env))

; apply-env: Env x Var -> SchemeVal
(define (apply-env env search-var)
  ; search through env, wrap results or errors
  (define (apply-env-aux env search-var)
    (cond ((eqv? (car env) 'empty-env)
            (cons 'not-found search-var))
          ((eqv? (car env) 'extend-env)
           (let ((saved-var (cadr env))
                 (saved-val (caddr env))
                 (saved-env (cadddr env)))
             (if (eqv? search-var saved-var)
               (cons 'found saved-val)
               (apply-env-aux saved-env search-var))))
          (else
            (cons 'invalid env))))
  (let ((result (apply-env-aux env search-var)))
    (case (car result)
      ((found)
        (cdr result))
      ((not-found) 
        (pretty-print env)
        (report-no-binding-found (cdr result)))
      ((invalid)
        (pretty-print env)
        (report-no-binding-found (cdr result))))))

; don't know what would be more informative
;   so I pretty print all bindings when an error is raised.
(define (pretty-print env)
  (define (print-binding var val)
    (display var)
    (display " = ")
    (out val))
  (out "-- environment begin --")
  (let loop ((env env))
    (if (eqv? (car env) 'empty-env)
      'done
      (begin
        (print-binding (cadr env) (caddr env))
        (loop (cadddr env)))))
  (out "-- environment end   --"))

(define (report-no-binding-found search-var)
  (eopl:error
    'apply-env
    "No binding for ~s"
    search-var))

(define (report-invalid-env env)
  (eopl:error
    'apply-env
    "Bad environment: ~s"
    env))

(require "./ex-2.6-test.rkt")

(do-env-test empty-env extend-env apply-env)

(define e
  (extend-env
    'x 1
    (extend-env
      'y 2
      (extend-env
        'z 3
        (empty-env)))))
(apply-env e 'w)
