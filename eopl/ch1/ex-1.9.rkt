#lang eopl

(require "../common.rkt")

; remove: Sym x Listof(Sym) -> Listof(Sym)
;   usage: remove all occurrences of `s`
(define (remove s los)
  (if (null? los)
    '()
    (let ((rest-results
            (remove s (cdr los))))
      (if (eqv? s (car los))
        rest-results
        (cons (car los)
              rest-results)))))

(out (remove 1 '(1 1 2 2 3 3 4 4 1)))
(out (remove 7 '(1 1 2 2 3 3 4 4 1)))
