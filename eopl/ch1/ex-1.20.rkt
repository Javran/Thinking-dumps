#lang eopl

(require "../common.rkt")

; count-occurrences :: (Sym, [SExp]) -> Int
; usage: count occurrences of `s` in `slist`
(define (count-occurrences s slist)
  ; count occurrence on a s-exp
  (define (count-occur-aux sexp)
    (if (symbol? sexp)
      (if (eq? s sexp) 1 0)
      (count-occurrences s sexp)))
  (my-foldl + 0
         (map count-occur-aux slist)))

(out (count-occurrences 'x '((f x) y (((x z) x))))
     (count-occurrences 'x '((f x) y (((x z) () x))))
     (count-occurrences 'w '((f x) y (((x z) x))))
     (count-occurrences 'f '((f x) y (((x z) x))))
     (count-occurrences 'y '((f x) y (((x z) x))))
     (count-occurrences 'z '((f x) y (((x z) x))))
     )
