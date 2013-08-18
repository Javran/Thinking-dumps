(define empty? ((curry2 apply-generic) 'empty?))

(define (to-order-coeff-list ls)
  (if (empty? ls)
    '()
    (let ((t (first-term ls)))
      (cons (order t)
            (cons (coeff t)
                  (to-order-coeff-list (rest-terms ls)))))))
