
(define (install-deriv-my-impl)
  ; constructors
  (define (make-sum a1 a2)
    (list '+ a1 a2))
  (define (make-product m1 m2)
    (list '* m1 m2))

  ; selectors
  (define addend car)
  (define augend cadr)
  (define multiplier car)
  (define multiplicand cadr)

  ; procedures to be exposed
  (define (deriv-plus exp var)
    (let ((e1 (addend exp))
          (e2 (augend exp)))
      (make-sum (deriv e1 var)
                (deriv e2 var))))
  (define (deriv-mul exp var)
    (let ((e1 (multiplier exp))
          (e2 (multiplicand exp)))
      (make-sum
        (make-product (deriv e1 var) e2)
        (make-product (deriv e2 var) e1))))

  ; install
  (put 'deriv '+ deriv-plus)
  (put 'deriv '* deriv-mul)

  (put 'make-sum 'sum-product make-sum)
  (put 'make-product 'sum-product make-product)
  )
