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
  (out (add a b)
       (sub a b)
       (mul a b)
       (div a b)))
(newline)

(end-script)
