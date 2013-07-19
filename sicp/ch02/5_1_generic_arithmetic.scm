(load "../common/utils.scm")

; basic supports from previous code
(load "./4_3_data_directed_put_get.scm")

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args))
        (error
          "No method for these types: APPLY-GENERIC"
          (list op type-tags))))))

; generic arithmetic procedures:
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

; TODO: we don't actually need to give the implementation of `sub` and `div`
;   and if we can find the negative of any valid number x, say `(neg x)`
;   then we can have (define (sub x y) (add x (neg y))).
;   Similarly, we can have `div` redefined.

(load "./5_1_generic_arithmetic_scheme_number.scm")
(install-scheme-number-package)
(define make-scheme-number
  (get 'make 'scheme-number))

; test scheme-number
(let ((a (make-scheme-number 20))
      (b (make-scheme-number 10)))
  (out (add a b) ;  30
       (sub a b) ;  10
       (mul a b) ; 200
       (div a b) ;   2
       ))

(load "./5_1_generic_arithmetic_rational.scm")
(install-rational-package)
(define make-rational
  (get 'make 'rational))

; test rational numbers
(newline)
(let ((a (make-rational 8 9))
      (b (make-rational 3 7)))
  (out (add a b) ; 83/63
       (sub a b) ; 29/63
       (mul a b) ;  8/21
       (div a b) ; 56/27
       ))

(load "./5_1_generic_arithmetic_complex.scm")
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
