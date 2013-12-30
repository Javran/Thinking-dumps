#lang eopl

(require "./common.rkt")

; invert :: [(a,b)] -> [(b,a)]
; usage: every element in `lst` should be 
;   a list of two elements
;   `(invert lst)` reverses each element list of `lst`
(define (invert lst)
  (map
    (lambda (pair)
      (list (cadr pair)
            (car  pair)))
    lst))

(out (invert '((a 1) (a 2) (1 b) (2 b)))
     (invert '((a 1) (b 2) (c 3) (d 4)))
     )
