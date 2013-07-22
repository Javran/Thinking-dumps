(load "../common/utils.scm")

; I think ex 2.80 should be done before ex 2.79
; because the mathmatical way of determining if two numbers are equal
; is to do substraction and judge if the result is close enough to zero.

(load "./4_3_data_directed_put_get.scm")
(load "./exercise_2_78_tag_system.scm")

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (=zero? x) (apply-generic '=zero? x))
; nothing more than just making use of `=zero?` and `sub`
(define (equ? x y)
  (=zero? (sub x y)))

(define eps 1e-7)

(load "./exercise_2_80_scheme_number.scm")
(install-scheme-number-package)
(define make-scheme-number
  (get 'make 'scheme-number))
(load "./exercise_2_80_rational.scm")
(install-rational-package)
(define make-rational
  (get 'make 'rational))
(load "./exercise_2_80_complex.scm")
(install-complex-package)
(define make-from-real-imag
  (get 'make-from-real-imag 'complex))
(define make-from-mag-ang
  (get 'make-from-mag-ang 'complex))

(define test-cases
  (list 
    ; scheme-number
    (cons (list (make-scheme-number 1) 
                (make-scheme-number 2))
          #f)
    (cons (list (make-scheme-number 123)
                (make-scheme-number 123))
          #t)

    ; rational
    (cons (list (make-rational 1 3)
                (make-rational 2 7))
          #f)
    (cons (list (add (make-rational 1 3)
                     (make-rational 1 6))
                (make-rational 1 2))
          #t)

    ; complex
    (cons (list (make-from-real-imag 3 4)
                (make-from-mag-ang 3 4))
          #f)
    (cons (list (make-from-real-imag (sqrt 3) 1)
                (make-from-mag-ang 2 (/ pi 6)))
          #t)))

(define (test-eq-zero test-case)
  (let ((input (car test-case))
        (answer (cdr test-case)))
    (if (equal? (apply equ? input) answer)
      (display ".")
      (error "Test failed on case:" test-case))))

(for-each
  test-eq-zero
  test-cases)
(out " All test passed.")

(end-script)
