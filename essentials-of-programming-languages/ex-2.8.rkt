#lang eopl

(require "./common.rkt")

; from "./ex-2.5.rkt"
(require "./ex-2.5.rkt")

; empty-env?: Env -> Bool
(define empty-env? null?)

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

(out (empty-env? e)
     (empty-env? (empty-env)))
; #f, #t

(provide empty-env?)
