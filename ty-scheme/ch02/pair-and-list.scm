(load "../common/utils.scm")

; cons combines things together
(define pair1 (cons 1 #t))
(define pair2 '(1 . #t))
(out pair1)
(out pair2)
; there are all '(1 . #t)

(out (car pair1))
; 1
(out (cdr pair1))
; #t

; car and cdr can be set as well
(set-car! pair1 2)
(set-cdr! pair1 #f)

(out pair1)

; dot pairs can contain pairs
(define y (cons (cons 1 2) 3))

(out y)
(out '((1 . 2) . 3))

(out (car (car y)))
(out (caar y))
; 1

(out (cdr (car y)))
(out (cdar y))
; 2

(out (cdr y))
; 3

(out (cons 1 (cons 2 (cons 3 (cons 4 5)))))
(out '(1 . (2 . (3 . (4 . 5)))))
(out '(1 2 3 4 . 5))
; (1 2 3 4 5)

(out (eqv? '(1 . 2) (cons 1 2)))
; why returns #f here?

(out '())
; empty

(out (cons 1 (cons 2 (cons 3 '()))))
(out '(1 2 3))
(out (list 1 2 3))
; (1 2 3)

; access a list by index
(define lst (list 1 2 3 4))
(out (list-ref lst 0))
; 1
(out (list-ref lst 3))
; 4

; from index to the last
(out (list-tail lst 0))
; (1 2 3 4)

(out (list-tail lst 3))
; (4)

; pair? list? null?

(out (pair? '(1 2)))
; #t
(out (pair? (list 1 2)))
; #t
(out (pair? '()))
; #f
(out (null? '()))
; #t
(out (null? (list)))
; #t
(out (list? '(1 2)))
; #t
(out (list? '(1 . 2)))
; #f (recall it ends with 2)
(out (list? '(1 . (2 . ()))))
; #t
(out (null? '(1 2)))
; #f
(out (null? '(1 . 2)))
; #f
