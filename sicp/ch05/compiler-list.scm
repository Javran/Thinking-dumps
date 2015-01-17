;; changed to use "member" because "memq" doesn't work
;; for more general cases
(define (list-union s1 s2)
  (cond ((null? s1) s2)
        ((member (car s1) s2) (list-union (cdr s1) s2))
        (else
         (cons (car s1) (list-union (cdr s1) s2)))))

(define (list-difference s1 s2)
  (cond ((null? s1) '())
        ((member (car s1) s2) (list-difference (cdr s1) s2))
        (else
         (cons (car s1)
               (list-difference (cdr s1) s2)))))
