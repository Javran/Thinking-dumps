
(define (install-deriv-my-exp-impl)
  ; data verifier and selectors
  (define (exponentiation? e)
    (and (non-empty? e)
         (eq? (car e) '**)))
  (define base car)
  (define exponent cadr)

  ; constructor
  (define (make-exponentiation e1 e2)
    (cond ((and (number? e2)
                (= e2 0)) 1)
          ((and (number? e2)
                (= e2 1)) e1)
          (else (list '** e1 e2))))

  ; component deriv-exp
  (define (deriv-exp exp var)
      (let* ((u (base exp))
             (n (exponent exp))
             (du/dx (deriv u var)))
        (make-product
          (make-product
            n
            (make-exponentiation u (- n 1)))
          du/dx)))

  ; install
  (put 'deriv '** deriv-exp)
  (put 'make-exponentiation 'exp make-exponentiation)
  )
