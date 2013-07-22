(load "../common/utils.scm")
(load "./5_2_coercion_base.scm")

(load "./5_1_generic_arithmetic_scheme_number.scm")
(install-scheme-number-package)

(load "./5_1_generic_arithmetic_complex.scm")
(install-complex-package)

(define make-complex-from-real-imag
  (get 'make-from-real-imag 'complex))

(define make-scheme-number
  (get 'make 'scheme-number))

(define (scheme-number->complex n)
  (make-complex-from-real-imag (contents n) 0))

(put-coercion 'scheme-number
              'complex
              scheme-number->complex)

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args))
        ; else
        (if (= (length args) 2)
          (let ((type1 (car type-tags))
                (type2 (cadr type-tags))
                (a1 (car args))
                (a2 (cadr args)))
            (let ((t1->t2 (get-coercion type1 type2))
                  (t2->t1 (get-coercion type2 type1)))
              (cond (t1->t2 (apply-generic op (t1->t2 a1) a2))
                    (t2->t1 (apply-generic op a1 (t2->t1 a2)))
                    (else (error "No method for these types"
                                 (list op type-tags))))))
          (error "No method for these type"
                 (list op type-tags)))))))

(define (add x y)
  (apply-generic 'add x y))

(let ((a (make-complex-from-real-imag 1 2))
      (b (make-complex-from-real-imag 3 4))
      (c (make-scheme-number 5)))
  (out (add a b) ;  4 + 6i
       (add c c) ; 10
       (add a c) ;  6 + 2i
       (add c b) ;  8 + 4i
       ))

(end-script)
