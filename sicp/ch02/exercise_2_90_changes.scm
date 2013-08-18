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

(define (install-polynomial-package-auto-conversion)
    (define variable car)
    (define term-list cdr)

    (define (check-variable-and f)
      (lambda (a b)
        (assert (same-variable? (variable a)
                                (variable b)))
        (f a b)))

    ; convert type according to the first argument
    (define (add-poly a b)
      (let ((ta (term-list a))
            (tb (term-list b)))
        (make-poly (variable a)
                   (add ta 
                        (to-poly-termlist-type 
                          tb (type-tag ta))))))

    ; convert type according to the first argument
    (define (mul-poly a b)
      (let ((ta (term-list a))
            (tb (term-list b)))
        (make-poly (variable a)
                   (mul ta 
                        (to-poly-termlist-type 
                          tb (type-tag ta))))))

    (put 'add '(polynominal polynominal) (check-variable-and add-poly))
    (put 'mul '(polynominal polynominal) (check-variable-and mul-poly))
    'done)

(install-polynomial-package-auto-conversion)
