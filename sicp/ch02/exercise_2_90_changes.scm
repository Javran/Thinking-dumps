(define (install-poly-termlist-conversion)
  
  (define empty-termlist? (get 'empty? '(poly-termlist)))
  (define first-term (get 'first-term '(poly-termlist)))
  (define rest-terms (get 'rest-terms '(poly-termlist)))

  (define (to-order-coeff-list l)
    (if (empty-termlist? l)
      '()
      (let ((t1 (first-term l)))
        (cons (order t1)
              (cons (coeff t1)
                    (to-order-coeff-list (rest-terms l)))))))

  (define (to-termlist-dense l)
    (apply (get 'make-from-args 'poly-termlist-dense)
           (to-order-coeff-list l)))


  (put 'to-termlist-dense '(poly-termlist) to-termlist-dense))

(install-poly-termlist-conversion)
