(define (install-polynomial-package)
  ; data structure tag: poly
  ; each "poly" is consisted by a single variable & term list
  ; accessors:
  ;   * variable
  ;   * term-list
  ; for variables:
  ;   * same-variable?: precondition of `add-poly` and `mul-poly`
  ; for terms:
  ;   * the-empty-term-list
  ;   * add-terms
  ;   * mul-terms
  ;   * adjoin-term: add new term into the list
  ; for poly:
  ;   * add-poly
  ;   * mul-poly

  (define make-poly cons)
  (define variable car)
  (define term-list cdr)

  (define variable? symbol?)
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (equal? v1 v2)))

  (define (add-terms l1 l2)
    (cond ((empty-termlist? l1) l2)
          ((empty-termlist? l2) l1)
          (else
            (let ((t1 (first-term l1))
                  (t2 (first-term l2)))
              (cond ((> (order t1) (order t2))
                      (adjoin-term
                        t1 (add-terms (rest-terms l1) l2)))
                    ((< (order t1) (order t2))
                      (adjoin-term 
                        t2 (add-terms l1 (rest-terms l2))))
                    (else
                      adjoin-term
                      (make-term (order t1)
                                 (add (coeff t1) (coeff t2)))
                      (add-terms (rest-terms l1)
                                 (rest-terms l2))))))))

  (define (mul-terms l1 l2)
    (if (empty-termlist? l1)
      (the-empty-termlist)
      (add-terms (mul-term-by-all-terms (first-term l1) l2)
                 (mul-terms (rest-terms l1) l2))))
  (define (mul-term-by-all-terms t1 l)
    (if (empty-termlist? l)
      (the-empty-termlist)
      (let ((t2 (first-term l)))
        (adjoin-term
          (make-term (+ (order t1) (order t2))
                     (mul (coeff t1) (coeff t2)))
          (mul-term-by-all-terms t1 (rest-terms l))))))

  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (add-terms (term-list p1)
                            (term-list p2)))
      (error "Polys not in same var: ADD-POLY"
             (list p1 p2))))

  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (mul-terms (term-list p1)
                            (term-list p2)))
      (error "Polys not in same var: MUL-POLY"
             (list p1 p2))))

  (put 'add '(polynominal polynominal) (tagged 'polynominal add-poly))
  (put 'mul '(polynominal polynominal) (tagged 'polynominal mul-poly))
  (put 'make 'polynominal (tagged 'polynominal make-poly))
  'done)
