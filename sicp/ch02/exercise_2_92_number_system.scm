; required packages:
; (load "../common/utils.scm")
; (load "../common/test-utils.scm")
; (load "./tag_system.scm")

(load "./number_system_rational.scm")
(load "./number_system_scheme_number.scm")
(load "./exercise_2_92_number_system_complex.scm")
(load "./number_system_native_num_test.scm")
(load "./exercise_2_92_number_system_coercion.scm")

(define eps 1e-9)

(install-rational-package)
(install-scheme-number-package)
(install-complex-package)
(install-native-num-test)
(install-coercion-test)

(define make-rational (get 'make 'rational))
(define make-scheme-number (get 'make 'scheme-number))
(define make-complex-ri (get 'make-ri 'complex))
(define make-complex-ma (get 'make-ma 'complex))

(define (add a b) (apply-generic 'add a b))
(define (sub a b) (apply-generic 'sub a b))
(define (mul a b) (apply-generic 'mul a b))
(define (div a b) (apply-generic 'div a b))
(define (equ? a b) (apply-generic 'equ? a b))

(define (numer a) (apply-generic 'numer a))
(define (denom a) (apply-generic 'denom a))
(define (real-part a) (apply-generic 'real-part a))
(define (imag-part a) (apply-generic 'imag-part a))
(define (magnitude a) (apply-generic 'magnitude a))
(define (angle a) (apply-generic 'angle a))

(define (=zero? a) (apply-generic '=zero? a))
(define (to-string a) (apply-generic 'to-string a))

(define (raise a) (apply-generic 'raise a))
(define (project a) (apply-generic 'project a))

(define (run-test tag) ((get 'test tag)))
(define (run-tests tag-list) (for-each run-test tag-list))
(define (run-all-test)
  (run-tests
            (list
              'tag-system
              'scheme-number-package
              'rational-package
              'complex-package
              'native-num
              'coercion-system
              ))
  (out "all tests are done."))
