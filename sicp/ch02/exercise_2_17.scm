(load "../common/utils.scm")

(define (last-pair ls)
  ; ls should at least be a pair
  (if (pair? ls)
    (if (null? (cdr ls))
      ; no more element
      ls
      ; else
      (last-pair (cdr ls)))
    (error "perform last-pair on an empty list")))

(out (last-pair (list 1 2 3))
     ; (3)
     (last-pair (list 1 2))
     ; (2)
     (last-pair (list 1))
     ; (1)
     (last-pair (list 23 72 149 34))
     ; (34)
     )

(end-script)
