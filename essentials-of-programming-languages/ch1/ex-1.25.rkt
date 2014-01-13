#lang eopl

(require "../common.rkt")

; exists? :: (a -> Bool, [a]) -> Bool
; usage: return #t if there is an element in `lst`
;   that satisfies `pred`, #f elsewise
(define (exists? pred lst)
  (if (null? lst)
    #f
    (or (pred (car lst))
        (exists? pred (cdr lst)))))

(out (exists? number? '(a b c 3 e))
     (exists? number? '(a b c d e))
     (exists? number? '(a a a)))
