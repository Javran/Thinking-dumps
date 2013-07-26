(load "../common/utils.scm")

(load "./4_3_data_directed_put_get.scm")
(load "./exercise_2_83_num_all.scm")

(define (add a b)
  (apply-generic 'add a b))

(define (sub a b)
  (apply-generic 'sub a b))

(define (raise x)
  (apply-generic 'raise x))

(install-integer-package)
(install-rational-package)
(install-real-package)
(install-complex-package)

(define make-integer (get 'make 'integer))
(define make-rational (get 'make 'rational))
(define make-real (get 'make 'real))
(define make-complex (get 'make 'complex))

(out (add (make-rational 1 10)
          (make-rational 2 20)))

(out (add (make-real 1.23)
          (make-real 4.56)))

(out (add (make-complex 12 34)
          (make-complex 56 78)))

(newline)
(out (raise (make-integer 10)))
(out (raise (make-rational 10 17)))
(out (raise (make-real 123)))

(end-script)
