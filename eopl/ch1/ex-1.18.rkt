#lang eopl

(require "../common.rkt")

; swapper :: (Sym, Sym, SList) -> SList
; usage: swap any occurence of `s1` to `s2`,
;   and swap any occurence of `s2` to `s1`.
(define (swapper s1 s2 slist)
  ; performs swapping on s-exp
  (define (swapper-aux sexp)
    (if (symbol? sexp)
      (cond ((eq? s1 sexp) s2)
            ((eq? s2 sexp) s1)
            (else sexp))
      (swapper s1 s2 sexp)))
  (map swapper-aux slist))

(out (swapper 'a 'd '(a b c d))
     (swapper 'a 'd '(a d () c d))
     (swapper 'x 'y '((x) y (z (x))))
     )
