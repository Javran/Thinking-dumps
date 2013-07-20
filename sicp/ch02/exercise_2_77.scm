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
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(load "./exercise_2_77_complex.scm")
(install-complex-package)
(define make-from-real-imag
  (get 'make-from-real-imag 'complex))
(define make-from-mag-ang
  (get 'make-from-mag-ang 'complex))

; test complex numbers
(newline)
(let* ((a (make-from-real-imag 1  4))
       (b (make-from-real-imag 2 -3))
       (real-part (lambda (x)
                    (apply-generic 'real-part (contents x))))
       (imag-part (lambda (x)
                    (apply-generic 'imag-part (contents x))))
       (mul-result (mul a b))
       (div-result (div a b)))
  (out (add a b)  ;      3+     i
       (sub a b)  ; -    1+    7i
       mul-result ;     14+    5i
       (real-part mul-result) 
       (imag-part mul-result)
       div-result ; -10/13+11/13i ~= -0.769+0.846i
       (real-part div-result) 
       (imag-part div-result)
       ))

(end-script)
