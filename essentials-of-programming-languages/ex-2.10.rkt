#lang eopl

(require "./common.rkt")

(require "./ex-2.5.rkt")
(require "./ex-2.8.rkt")
(require "./ex-2.9.rkt")

(out "------")

(define (extend-env* vars vals env)
  (foldl
    (lambda (cur-env k-v)
      (extend-env
        (car k-v)
        (cdr k-v)
        cur-env))
    env
    (map cons vars vals)))

(define e
  (extend-env*
    '(x y z)
    '(1 2 3)
    (empty-env)))

(for-each
  out
  (map
    ((curry2 apply-env) e)
    '(x y z)))
