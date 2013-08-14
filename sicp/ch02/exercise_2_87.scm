(load "../common/utils.scm")
(load "../common/test-utils.scm")
(load "./tag_system.scm")
(load "./number_system.scm")

(load "./5_3_polynominal_package.scm")
(install-polynomial-package)

(define variable? symbol?)
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (equal? v1 v2)))

(load "./5_3_poly_term_package.scm")
(load "./5_3_poly_termlist_package.scm")
(install-poly-term-package)
(install-poly-termlist-package)

(define make-term (get 'make 'poly-term))
(define (order x) (apply-generic 'order x))
(define (coeff x) (apply-generic 'coeff x))

(run-test 'poly-term-package)

(define make-termlist-from-args
  (get 'make-from-args 'poly-termlist))
(define make-poly
  (get 'make 'polynominal))
(define the-empty-term-list
  (get 'make 'poly-termlist))

; ==== solution start ====

; the original =zero? is implemented as returning #f directly
;   which will fail to recognize zero when it is presented by polynominals
; for example, if we replace 1 with (1)x^0 and replace -1 with (-1)x^0
;   in (x+1) * (x-1)
;   the result will have the term "(0)x^1".
(define (test-poly-=zero?)
  (let* ((pos-one (make-poly
                    'x
                    (make-termlist-from-args
                      0 (make-scheme-number 1))))
         (neg-one (make-poly
                    'x
                    (make-termlist-from-args
                      0 (make-scheme-number -1))))
         (p1 (make-poly
               'x
               (make-termlist-from-args
                 1 pos-one
                 0 pos-one)))
         (p2 (make-poly
               'x
               (make-termlist-from-args
                 1 pos-one
                 0 neg-one))))
    (out (to-string (mul p1 p2)))))

(test-poly-=zero?)
; output: ((1)x^0)x^2+(0)x^1+((-1)x^0)x^0

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

; now we re-run the test
(test-poly-=zero?)
; output: ((1)x^0)x^2+((-1)x^0)x^0
; "(0)x^1" gets removed

(end-script)
