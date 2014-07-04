;; if two sets are equal
;; (order is not taken into account)
(define (set-equal? s1 s2)
  (cond ((null? s1) (null? s2))
        ((null? s2) (null? s1))
        (else
         (set-equal?
          (cdr s1)
          (delete (car s1) s2)))))

(do-test
 set-equal?
 (list
  (mat '(1 2 3) '(3 2 1) #t)
  (mat '(1 (a b (c (d))))
       '((a b (c (d))) 1) #t)
  (mat '() '(1 2) #f)
  (mat '(1 2) '() #f)))
