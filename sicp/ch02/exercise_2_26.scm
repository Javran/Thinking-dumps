(load "../common/utils.scm")

(define x (list 1 2 3))
(define y (list 4 5 6))

; (error "comment this line for results")

(out (append x y))
; guess: (1 2 3 4 5 6)
(out (cons x y))
; guess: ((1 2 3) 4 5 6)
(out (list x y))
; guess: ((1 2 3) (4 5 6))

(end-script)
