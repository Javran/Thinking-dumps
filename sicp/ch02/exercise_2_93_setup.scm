(load "./tag_system.scm")
(load "./exercise_2_92_number_system.scm")
(load "./exercise_2_92_setup.scm")

(load "./exercise_2_93_number_system_rational_p.scm")

(install-rational-p-package)

(define make-rational-p 
  (get 'make 'rational-p))
