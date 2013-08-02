(load "../common/utils.scm")

; changes to the system needed to accommodate this:
; * modify constructors to prevent building a complex number
;   from complex numbers
; * use `add` `sub` ... instead of `+` `-` to deal with mathmatical operations
; * insert `sine` and `cosine` to take the place of `sin` and `cos`
; * (optional) implement `print-num` to visualize a number


; test tools
(load "./exercise_2_86_test.scm")

; tag system
(load "./exercise_2_86_tag_system.scm")

; math precision
(define eps 1e-7)

; num packages
(load "./exercise_2_86_pkg_integer.scm")
(load "./exercise_2_86_pkg_rational.scm")
(load "./exercise_2_86_pkg_real.scm")
(load "./exercise_2_86_pkg_complex.scm")

(install-integer-package)
(install-rational-package)
(install-real-package)
(install-complex-package)

; package requirement:
; * constructors
(define make-integer (get 'make 'integer))
(define make-rational (get 'make 'rational))
(define make-real (get 'make 'real))
(define make-complex-ri (get 'make-from-real-imag 'complex))
(define make-complex-ma (get 'make-from-mag-ang 'complex))

; * accessors:
;   denom/numer for rational
;   real-part/imag-part/magnitude/angle for complex numbers
(define (denom x) (apply-unary 'denom x))
(define (numer x) (apply-unary 'numer x))
(define (real-part x) (apply-unary 'real-part x))
(define (imag-part x) (apply-unary 'imag-part x))
(define (magnitude x) (apply-unary 'magnitude x))
(define (angle x) (apply-unary 'angle x))

; * mathematical operations:
;   add/sub/mul/equ?/=zero?
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (equ? x y) (apply-generic 'equ? x y))
(define (=zero? x) (apply-unary '=zero? x))

; * coercion system:
;   raise/project
(define (raise x) (apply-unary 'raise x))
(define (project x) (apply-unary 'project x))

; * (optional) visualization
;   print-num
(define (print-num x) (apply-unary 'print-num x))

(test-integer-package)
(test-rational-package)
(test-real-package)
(test-complex-package)

(define (assert x)
  (if x
    (display ".")
    (error "Assertion failed")))

(display "Testing coercion for apply-generic ...")
(assert (equ? (add (make-complex-ri 7 0) (make-real -7.0))
              (make-integer 0)))
(assert (equ? (sub (make-rational 1 4) (make-integer 1))
              (make-real -0.75)))
(assert (equ? (add (make-integer 1) (make-complex-ri 1 0))
            (make-rational 10 5)))
(assert (equ? (mul (make-integer 1) (make-complex-ri 0 1))
            (make-complex-ma 1 (/ pi 2))))
(out "Test passed.")

(display "Testing drop ... ")
(test-drop)

; switch to new `apply-generic` (i.e. with `drop` feature)
(define apply-generic apply-generic-d)

(display "Testing apply-generic(with drop) ")
(let ((testcases (list
                   (cons (list (add (make-complex-ri (sqrt 3) 1)
                                    (make-complex-ma 1 (* (/ 3 2) pi))))
                         'rational)
                   (cons (list (mul (make-complex-ri 1 1)
                                    (make-complex-ma (* 2 (sqrt 2)) (- (/ pi 4)))))
                         'integer)
                   (cons (list (sub (make-rational 1234 25)
                                    (make-real 49.36)))
                         'integer)
                   (cons (list (sub (make-complex-ri 10 0)
                                    (make-rational 2 5)))
                         'rational))))
  (do-test type-tag testcases))

(load "./exercise_2_86_pkg_complex2.scm")
(install-complex2-package)

; changes to the system:
; * complex2 (i.e. the complex number made from other real numbers) 
;   should be called with instances of numbers instead of primitive numbers
; * for accessors, real-part & imag-part works perfectly for rect-impl
;   and magnitude & angle works perfectly for polar-impl
;   so we need to get another 4 accessors work

(out "===================")
(define make-complex2-ri (get 'make-from-real-imag 'complex2))
(define make-complex2-ma (get 'make-from-mag-ang 'complex2))

(let ((c1 (make-complex2-ri (make-rational 2 1) (make-real 2.0)))
      (c2 (make-complex2-ma (make-integer 2) (make-real (/ pi 2)))))
  (out c1)
  (out  (real-part c1) (imag-part c1))
  (out "===================")
  (out c2)
  (out (magnitude c2) (angle c2))

  )

(end-script)
