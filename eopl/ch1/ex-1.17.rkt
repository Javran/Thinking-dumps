#lang eopl

(require "../common.rkt")

; down :: [a] -> [[a]]
; usage: wrap parentheses around each element of `lst`
(define (down lst)
  (map list lst))

(out (down '(1 2 3))
     (down '((a) (fine) (idea)))
     (down '(a (more (complicated)) object))
     (down '(foo () bar ()))
     )
