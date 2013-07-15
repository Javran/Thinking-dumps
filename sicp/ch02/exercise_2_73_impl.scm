
(define (install-deriv-my-impl)
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

  (put 'deriv '+ deriv-plus)
  (put 'deriv '* deriv-mul)
  )
