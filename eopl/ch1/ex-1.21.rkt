#lang eopl

(require "../common.rkt")

; product :: ([a], [b]) -> [(a,b)]
; usage: return the Cartesian product of `sos1` and `sos2`
(define (product sos1 sos2)
  (concat
    (map
      (lambda (fst)
        (map (lambda (snd)
               (list fst snd))
             sos2))
      sos1)))

(out (product '(a b c) '(x y))
     (product '(1 2 3) '(4 5 6 7))
     )
