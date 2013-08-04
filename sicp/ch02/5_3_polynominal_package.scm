(define (install-polynomial-package)
  ;; internal procedures
  ;; representation of poly
  (define make-poly cons)
  (define variable car)
  (define term-list cdr)

  (define variable? symbol?)
  (define (same-varialbe? v1 v2)
    (and (variable? v1) (variable? v2) (equal? v1 v2)))

  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (add-terms (term-list p1)
                            (term-list p2)))
      (error "Polys not in same ver: ADD-POLY"
             (list p1 p2))))

  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (mul-terms (term-list p1)
                            (term-list p2)))
      (error "Polys not in same ver: MUL-POLY"
             (list p1 p2))))

  (put 'add '(polynominal polynominal) (tagged 'polynominal add-poly))
  (put 'mul '(polynominal polynominal) (tagged 'polynominal mul-poly))
  (put 'make 'polynominal (tagged 'polynominal make-poly))
  'done)
