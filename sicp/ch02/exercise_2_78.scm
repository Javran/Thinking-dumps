(load "../common/utils.scm")
(load "./4_3_data_directed_put_get.scm")
(load "./5_1_generic_arithmetic_scheme_number.scm")
(install-scheme-number-package)

(define make-scheme-number
  (get 'make 'scheme-number))

(define (test-number)
  (let ((n (make-scheme-number 2)))
    (display "dump: ") (display n) (newline)
    (display "type-tag: ") (display (type-tag n)) (newline)
    (display "contents: ") (display (contents n)) (newline)))

; before modification
(test-number)

(load "./exercise_2_78_tag_system.scm")

; after modification
(newline)
(test-number)

(end-script)
