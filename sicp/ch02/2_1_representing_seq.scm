(load "../common/utils.scm")

(define one-through-four-1 (list 1 2 3 4))
(define one-through-four-2 '(1 2 3 4))
(define one-through-four-3 (cons 1 (cons 2 (cons 3 (cons 4 '())))))

(out one-through-four-1
     one-through-four-2
     one-through-four-3)

(out (equal? one-through-four-1 one-through-four-2)
     (equal? one-through-four-2 one-through-four-3)
     (equal? one-through-four-1 one-through-four-3))

(out (car one-through-four-1)
     ; 1
     (cdr one-through-four-1)
     ; (2 3 4)
     (car (cdr one-through-four-1))
     ; 2
     (cons 10 one-through-four-1)
     ; (10 1 2 3 4)
     (cons 5 one-through-four-1)
     ; (5 1 2 3 4)
     )

(define (my-list-ref items n)
  (if (= n 0)
    (car items)
    (my-list-ref (cdr items) (- n 1))))

(define squares (map square '(1 2 3 4 5)))
(out squares
     ; (1 4 9 16 25)
     (my-list-ref squares 3)
     ; 16
     )

(end-script)
