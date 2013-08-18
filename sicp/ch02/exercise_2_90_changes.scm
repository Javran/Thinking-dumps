(define empty? ((curry2 apply-generic) 'empty?))

(define (to-order-coeff-list ls)
  (if (empty? ls)
    '()
    (let ((t (first-term ls)))
      (cons (order t)
            (cons (coeff t)
                  (to-order-coeff-list (rest-terms ls)))))))

(define (to-poly-termlist-type termlist type)
  (if (eq? type (type-tag termlist))
    termlist
    (apply (get 'make-from-args type)
           (to-order-coeff-list termlist))))
