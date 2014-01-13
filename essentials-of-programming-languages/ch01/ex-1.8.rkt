#lang eopl

(require "./common.rkt")

; drop-until-found: Sym * Listof(Sym) -> Listof(Sym)
;   usage: drop elements from the beginning of `los`,
;     until the first element that equal to `s` is found.
;     this first element is also dropped.
(define (drop-until-found s los)
  (if (null? los)
    '()
    (if (eqv? (car los) s)
      (cdr los)
      (drop-until-found s (cdr los)))))

(let ((l '(1 2 2 3 4 4 5)))
  (for-each
    (lambda (x) (out (drop-until-found x l)))
    '(2 4 6)))
