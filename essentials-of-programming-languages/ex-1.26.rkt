#lang eopl

(require "./common.rkt")

; up :: [Either a [a]] -> [a]
; usage: remove a pair of parentheses for each element
;   of `lst`, the element does not change
;   if the element is not a list.
(define (up lst)
  ; make sure the return value is a list
  (define (ensure-list x)
    (if (list? x)
      x
      (list x)))
  (foldl append '() (map ensure-list lst)))

(out (up '((1 2) (3 4)))
     (up '((x (y)) z))
     (up (up '(((x)) y (z) ))))
