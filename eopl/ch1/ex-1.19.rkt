#lang eopl

(require "../common.rkt")

; list-set :: ([a], Int, a) -> [a]
; usage: replace the `n`-th element (zero-indexed)
;   with `x`
(define (list-set lst n x)
  (cond ((null? lst) lst)
        ((< n 0) lst)
        ((= n 0) (cons x (cdr lst)))
        (else (cons (car lst)
                    (list-set (cdr lst) (- n 1) x)))))

(out (list-set '(a b c d) 2 '(1 2))
     (list-ref (list-set '(a b c d) 3 '(1 5 10)) 3)
     (list-set '(a b c d) 10 'x)
     (list-set '(a b c d) -10 'x)
     )
