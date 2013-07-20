(load "../common/utils.scm")

(load "./4_3_data_directed_put_get.scm")
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args))
        (error
          "No method for these types: APPLY-GENERIC"
          (list op type-tags))))))
(load "./exercise_2_77_complex.scm")
(install-complex-package)
(define make-from-real-imag
  (get 'make-from-real-imag 'complex))
(define make-from-mag-ang
  (get 'make-from-mag-ang 'complex))

(define (real-part x) (apply-generic 'real-part x))
(define (imag-part x) (apply-generic 'imag-part x))
(define (magnitude x) (apply-generic 'magnitude x))
(define (angle x) (apply-generic 'angle x))

(define (show-info x)
  (display "data dump: ") (display x) (newline)
  (display "real-part: ") (display (real-part x)) (newline)
  (display "imag-part: ") (display (imag-part x)) (newline)
  (display "magnitude: ") (display (magnitude x)) (newline)
  (display "angle: ") (display (angle x)) (newline))

; test complex numbers
(newline)
(let* ((a (make-from-real-imag 3 4))
       (b (make-from-mag-ang   3 4)))
  (show-info a)
  (show-info b)
  )
  (end-script)
