; changes to the poly system

; * we can implement =zero? in poly-termlist package
;   because it has better understanding of zero than the polynominal package
; * it's guaranteed that coeff in the termlist cannot be zero (by adjoin-term)
;   so the termlist is zero if and only if the termlist is empty
(put '=zero? '(poly-termlist) (get 'empty? '(poly-termlist)))
(put '=zero? '(polynominal)
     (lambda (p)
       (apply-generic 
         '=zero? 
         ; call inside the package, shouldn't use apply-generic
         ((get 'term-list '(polynominal)) p))))
