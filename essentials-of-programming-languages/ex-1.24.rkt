#lang eopl

(require "./common.rkt")

; every? :: (a -> Bool) -> [a] -> Bool
; usage: return #t if all elements of `lst`
;   satisfies `pred`, #f elsewise
(define (every? pred lst)
  (if (null? lst)
    #t
    (and (pred (car lst))
         (every? pred (cdr lst)))))

(out (every? number? '(a b c 3 e))
     (every? number? '(1 2 3 5 4))
     (every? number? '(1)))
