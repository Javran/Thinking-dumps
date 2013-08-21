;(load "./5_3_polynominal_setup.scm")
;(load "./exercise_2_87_changes.scm")
;(load "./exercise_2_88_changes.scm")
;(load "./exercise_2_89_changes.scm")
;(load "./exercise_2_90_changes.scm")
;(load "./exercise_2_91_changes.scm")

(define variable? symbol?)
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (equal? v1 v2)))

(load "./exercise_2_92_poly_term_package.scm")
(load "./exercise_2_92_polynominal_package.scm")

(install-poly-term-package)
(install-polynomial-package)

(define make-poly (get 'make 'polynominal))
(define variable ((curry2 apply-generic) 'variable))
(define term-list ((curry2 apply-generic) 'term-list))

(run-tests 
  (list 
    'polynominal-package
    ))
