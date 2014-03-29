#lang eopl

(require "../common.rkt")

(require "./ex-2.5.rkt")
(require "./ex-2.8.rkt")

(define e
  (extend-env
    'x 1
    (extend-env
      'y 2
      (empty-env))))

(provide has-binding?)

; has-binding?: Env x Var -> Bool
(define (has-binding? env s)
  (assv s env))

(for-each
  out
  (map ((curry2 has-binding?) e)
       '(x y z)))
