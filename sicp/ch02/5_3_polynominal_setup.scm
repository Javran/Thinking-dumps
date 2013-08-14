; setup environment for full polynominal package

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
