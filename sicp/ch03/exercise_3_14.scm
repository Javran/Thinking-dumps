(load "../common/utils.scm")
(load "../common/test-utils.scm")

(define (mystery x)
  (define (loop x y)
    (if (null? x)
      y
      (let ((temp (cdr x)))
        (set-cdr! x y)
        (loop temp x))))
    (loop x '()))

; I guess this code returns the reversed list, destructively.
(define (my-reverse! ls)
  (define (loop ls result-ls)
    (if (null? ls)
      result-ls
      (let ((temp (cdr ls)))
        (set-cdr! ls result-ls)
        (loop temp ls))))
    (loop ls '()))

(define a '(1 2 3 4 5))
(define b '(1 2 3 4 5))
(out (mystery a))
; (5 4 3 2 1)
(out (my-reverse! b))
; (5 4 3 2 1)
(out a b)
; show that this is destructive
; (1)
; (1)

(define v (list 'a 'b 'c 'd))
; TODO: draw the box-and-pointer diagram
(define w (mystery v))

(out v)
; (a)
(out w)
; (d c b a)

(end-script)
