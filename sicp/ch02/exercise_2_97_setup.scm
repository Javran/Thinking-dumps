(load "./exercise_2_96_setup.scm")

(load "./exercise_2_97_polynominal_package.scm")
(load "./exercise_2_97_poly_termlist_sparse_package.scm")

(install-polynomial-package)
(install-poly-termlist-sparse-package)

(run-test 'polynominal-package)
(run-test 'poly-termlist-sparse-package)

(define (reduce n d)
  (apply-generic 'reduce n d))
