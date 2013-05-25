(load "../common/utils.scm")

; "fringe" will flatten a tree and place all of its elements in order
(define (fringe tree)
  (if (list? tree)
    ; it's a list, we walk through it and apply "fringe" on each of the elements
    (if (null? tree)
      ; an empty list
      nil
      ; else
      (append (fringe (car tree)) (fringe (cdr tree))))
    ; else
    (list tree)))

(define x (list (list 1 2) (list 3 4)))

(out (fringe x))

(out (fringe (list x x)))

(define y (list (list 1) 
                (list (list 2 3 4)
                      (list 5)
                      (list))
                (list 6
                      (list 7
                            (list 8))
                      (list 9))))

(out (fringe y))
; (1 2 3 4 5 6 7 8 9)

(end-script)
