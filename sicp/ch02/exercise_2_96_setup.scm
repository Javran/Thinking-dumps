(load "./exercise_2_95_setup.scm")

(load "./exercise_2_96_polynominal_package.scm")
(load "./exercise_2_96_poly_termlist_sparse_package.scm")

(install-polynomial-package)
(install-poly-termlist-sparse-package)

(run-test 'polynominal-package)
(run-test 'poly-termlist-sparse-package)

(define (pseudoremainder a b)
  (apply-generic 'pseudoremainder a b))
