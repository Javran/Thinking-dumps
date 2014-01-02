#lang eopl

(require "./common.rkt")

; flatten :: SList -> [Sym]
; usage: flatten `slist` to a list of symbols
;   the order of symbol is kept during flattening
(define (flatten slist)
  (define (flatten-exp e)
    (if (symbol? e)
      (list e)
      (flatten e)))
  (concat-map flatten-exp slist))

(out (flatten '(a b c))
     (flatten '((a) () (b ()) () (c)))
     (flatten '((a b) c (((d)) e)))
     (flatten '(a b (() (c))))
     (flatten '(a (b (c (d (e (f ())))))))
     )
