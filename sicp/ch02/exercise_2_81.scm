(load "../common/utils.scm")
(load "./5_2_coercion_base.scm")

(load "./exercise_2_81_scheme_number.scm")
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

(define (exp x y)
  (apply-generic 'exp x y))

; add output when either "t1->t2" or "t2->t1" get applied
; so when conversion happens we'll know
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
              (cond (t1->t2 
                      (out "t1->t2")
                      (apply-generic op (t1->t2 a1) a2))
                    (t2->t1
                      (out "t2->t1")
                      (apply-generic op a1 (t2->t1 a2)))
                    (else (error "No method for these types"
                                 (list op type-tags))))))
          (error "No method for these type"
                 (list op type-tags)))))))

(define (add x y)
  (apply-generic 'add x y))

(let ((a (make-complex-from-real-imag 1 2))
      (b (make-complex-from-real-imag 3 4))
      (c (make-scheme-number 5)))
  (out (add a b)) ;  4 + 6i
  (out (add c c)) ; 10
  (out (add a c)) ;  6 + 2i
  (out (add c b)) ;  8 + 4i
  )
; all possible situations have been tested,
; but no t1->t2->t1 conversion is observed

; I think the "cond" in "apply-generic" ensures "t1->t2" or "t2->t1" to be run exclusively.
; so no cycling type conversion would happen (unless there's a type-conversion chain and
; we don't have a suitable procedure to handle the operation)

(define (louis-reasoner-test)
  (put-coercion 'scheme-number 'scheme-number identity)
  (put-coercion 'complex 'complex identity)
  (newline)
  (out (add (make-scheme-number 1) (make-scheme-number 2))) ; run successfully
  (out (exp (make-scheme-number 2) (make-scheme-number 4))) ; run successfully
  (out (exp (make-complex-from-real-imag 2 0)
            (make-complex-from-real-imag 4 0))) ; run into an infinite loop
  )

; uncomment to run the test (warning: infinite loop)
; (louis-reasoner-test)

; answer to question a:
; it will run into an infinite loop because we don't have the corresponding
; procedure to process complex numbers but `apply-generic` tries to
; apply an identity conversion and nothing has changed 
; between each recursive calls to apply-generic

; answer to question b:
; apply-generic works correctly as is.

; answer to question c:
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (define (raise-error)
        (error "No method for these types"
               (list op type-tags)))
      (if proc
        (apply proc (map contents args))
        ; else
        (if (= (length args) 2)
          (let ((type1 (car type-tags))
                (type2 (cadr type-tags))
                (a1 (car args))
                (a2 (cadr args)))
            (if (equal? type1 type2)
              (raise-error)
              ; not of the same type
              (let ((t1->t2 (get-coercion type1 type2))
                    (t2->t1 (get-coercion type2 type1)))
                (cond (t1->t2 
                        (out "t1->t2")
                        (apply-generic op (t1->t2 a1) a2))
                      (t2->t1
                        (out "t2->t1")
                        (apply-generic op a1 (t2->t1 a2)))
                      (else (raise-error))))))
          (raise-error))))))

; uncomment to run the test
; (louis-reasoner-test)
; this time it will run into an error directly


(end-script)
