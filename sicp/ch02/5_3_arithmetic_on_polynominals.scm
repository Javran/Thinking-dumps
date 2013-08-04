(load "../common/utils.scm")
(load "./exercise_2_86_tag_system.scm")
(load "./5_3_polynominal_package.scm")

(install-polynomial-package)

(define (make-poly v t)
  ((get 'make 'polynominal) v t))


(end-script)
