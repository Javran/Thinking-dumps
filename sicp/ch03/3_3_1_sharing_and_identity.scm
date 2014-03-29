(load "../common/utils.scm")
(load "../common/test-utils.scm")

(define x (list 'a 'b))
(define z1 (cons x x))
(define z2 (cons (list 'a 'b) (list 'a 'b)))

(out z1)
(out z2)
; ((a b) a b)

(define (set-to-wow! x)
  (set-car! (car x) 'wow)
  x)

(out (eq? (car z1) (cdr z1)))
; #t
(out (eq? (car z2) (cdr z2)))
; #f

(set-to-wow! z1)
(set-to-wow! z2)

(out z1)
; ((wow b) wow b)
(out z2)
; ((wow b) a b)

(end-script)
