;(load "./5_3_polynominal_setup.scm")
;(load "./exercise_2_87_changes.scm")
;(load "./exercise_2_88_changes.scm")
;(load "./exercise_2_89_changes.scm")
;(load "./exercise_2_90_changes.scm")
;(load "./exercise_2_91_changes.scm")

(define variable? symbol?)
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (equal? v1 v2)))

(load "./exercise_2_92_polynominal_package.scm")
(install-polynomial-package)

(run-tests 
  (list 
    'polynominal-package
    ))
