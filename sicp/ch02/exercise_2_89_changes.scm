(define (install-poly-termlist-dense-package)
  ; for dense poly representation,
  ;   we define the underlying list should have at least a non-zero element 
  ;   and the first element (i.e. one that has the highest order) cannot be zero
  ; so the zero for this representation is nil
  (define (make-empty) nil)
  (define the-empty-termlist make-empty)
  (define empty-termlist? null?)

  (define (first-term ls)
    ; to obtain a term, we should combine the coeff with its order
    (let ((term-order (- (length ls) 1))
          (term-coeff (car ls)))
      ((get 'make 'poly-term) term-order term-coeff)))
  (define rest-terms cdr)

  (define coeff ((curry2 apply-generic) 'coeff))
  (define order ((curry2 apply-generic) 'order))

  (put 'make 'poly-termlist-dense (tagged 'poly-termlist-dense make-empty))
  (put 'first-term '(poly-termlist-dense) first-term)
  (put 'rest-terms '(poly-termlist-dense) (tagged 'poly-termlist-dense rest-terms))
  (put 'empty? '(poly-termlist-dense) empty-termlist?)

  'done)

(install-poly-termlist-dense-package)
