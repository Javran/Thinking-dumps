(load "../common/utils.scm")

; I think ex 2.80 should be done before ex 2.79
; because the mathmatical way of determining if two numbers are equal
; is to do substraction and judge if the result is close enough to zero.

(load "./4_3_data_directed_put_get.scm")
; use new tag system from ex 2.78
(load "./exercise_2_78_tag_system.scm")

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (=zero? x) (apply-generic '=zero? x))

; all packages will share a threshold: eps = 1e-7
; when abs(x) < eps, we'll consider x to be zero
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
    (cons (make-scheme-number 1) #f)
    (cons (make-scheme-number 0) #t)
    (cons (make-scheme-number 1e-6) #f)
    (cons (make-scheme-number 1e-8) #t)

    ; rational
    (cons (make-rational 1 10) #f)
    (cons (make-rational 1 10000000) #f)
    (cons (make-rational 1 100000000) #t)

    ; complex
    (cons (make-from-real-imag 0 0) #t)
    (cons (make-from-mag-ang 1e-4 pi) #f)
    (cons (sub (make-from-real-imag (sqrt 3) 1)
               (make-from-mag-ang 2 (/ pi 6))) #t)))

(define (test-eq-zero test-case)
  (let ((input (car test-case))
        (answer (cdr test-case)))
    (if (equal? (=zero? input) answer)
      (display ".")
      (error "Test failed on case:" test-case))))

(for-each
  test-eq-zero
  test-cases)
(out " All test passed.")

(end-script)
