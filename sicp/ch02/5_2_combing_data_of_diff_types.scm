(load "../common/utils.scm")

(load "./4_3_data_directed_put_get.scm")
(load "./exercise_2_78_tag_system.scm")

(load "./5_1_generic_arithmetic_scheme_number.scm")
(install-scheme-number-package)

(load "./5_2_combing_data_of_diff_types_complex.scm")
(install-complex-package)

(define make-from-real-imag
  (get 'make-from-real-imag 'complex))
(define make-scheme-num
  (get 'make 'scheme-number))

(define (add x y)
  (apply-generic 'add x y))

(out (add (make-from-real-imag 1 2)
          5))
; 1+2i + 5 = 6+2i

; disadvantages of designing a different procedure for each possible combination of types
; * the cost of introducing a new type raises
; * undermines our ability to combine separate packages additively
; * individual packages need to take account of other packages

(end-script)
