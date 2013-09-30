(load "./exercise_2_93_setup.scm")

(load "./exercise_2_94_polynominal_package.scm")
(load "./exercise_2_94_poly_termlist_sparse_package.scm")

(define (my-remainder t1 t2)
  (apply-generic 'remainder t1 t2))

(install-polynomial-package)
(install-poly-termlist-sparse-package)

(run-test 'polynominal-package)
(run-test 'poly-termlist-sparse-package)
