#lang eopl

(require "../common.rkt")

; duple :: (Int, SExp) -> [SExp]
; usage: return a list of `n` elements
;   the elements of whom are all `x`.
(define (duple n x)
  (if (= n 0)
    '()
    (cons x (duple (- n 1) x))))

(out (duple 2 3)
     (duple 4 '(ha ha))
     (duple 0 '(blah))
     (duple 2 '(foo (bar) ()))
     )
