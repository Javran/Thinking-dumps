(define (install-complex-package)
  (load "./number_system_rect.scm")
  (load "./number_system_polar.scm")
  (install-rect-package)
  (install-polar-package)

  (define make-ri (get 'make 'rect))
  (define make-ma (get 'make 'polar))

  (define (real-part x) (apply-generic 'real-part x))
  (define (imag-part x) (apply-generic 'imag-part x))
  (define (magnitude x) (apply-generic 'magnitude x))
  (define (angle x) (apply-generic 'angle x))

  (define (add x y)
    (make-ri (+ (real-part x) (real-part y))
             (+ (imag-part x) (imag-part y))))
  (define (neg x)
    (make-ri (- (real-part x)) (- (imag-part x))))
  (define (sub x y) (add x (neg y)))
  (define (mul x y)
    (make-ma (* (magnitude x) (magnitude y))
             (+ (angle x) (angle y))))
  (define (inv x)
    (make-ma (/ 1 (magnitude x))
             (- (angle x))))
  (define (div x y) (mul x (inv y)))

  (define (=zero? x) (< (magnitude x) eps))

  (define (equ? x y) (=zero? (sub x y)))

  (define (to-string x)
    (string-append
      (number->string (real-part x)) "+"
      (number->string (imag-part x)) "i"))

  (define (test-package)
    (run-test 'rect-package)
    (run-test 'polar-package)
    (let* ((make-ri (get 'make-ri 'complex))
           (make-ma (get 'make-ma 'complex))
           (x1 (make-ri (sqrt 3) 1))
           (x2 (make-ma 2 (/ pi 6)))
           (x3 (make-ri 2 -2))
           (x4 (make-ma 2 (/ pi 2))))
      ; test accessors
      (let ((testcases
              (list (mat 'real-part x1 (sqrt 3))
                    (mat 'real-part x2 (sqrt 3))
                    (mat 'imag-part x1 1)
                    (mat 'imag-part x2 1)
                    (mat 'magnitude x1 2)
                    (mat 'magnitude x2 2)
                    (mat 'angle x1 (/ pi 6))
                    (mat 'angle x2 (/ pi 6))
                    )))
        (do-test-q apply-generic testcases (close-number? eps)))
      ; test equ? and =zero?
      (let ((testcases
              (list (mat 'equ? x1 x2 #t)
                    (mat 'equ? x1 x3 #f)
                    (mat 'equ? x2 x3 #f)
                    (mat '=zero? (make-ri 0 0) #t)
                    (mat '=zero? (make-ma 2 0) #f))))
        (do-test-q apply-generic testcases))
      ; test math operations
      (let ((testcases
              (list (mat 'add x1 x2 (make-ma 4 (/ pi 6)))
                    (mat 'add x3 x4 (make-ri 2 0))
                    (mat 'sub x1 x2 (make-ri 0 0))
                    (mat 'sub x3 x4 (make-ri 2 -4))
                    (mat 'mul x1 x2 (make-ma 4 (/ pi 3)))
                    (mat 'div x3 x4 (make-ri -1 -1))
                    )))
        (do-test-q apply-generic testcases equ?))
      ; test to-string
      (let ((testcases
              (list (mat 'to-string x3 "2+-2i")
                    (mat 'to-string (make-ri 1 2) "1+2i"))))
        (do-test-q apply-generic testcases))
      ))

  (put 'make-ri 'complex (tagged 'complex make-ri))
  (put 'make-ma 'complex (tagged 'complex make-ma))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  (put 'add '(complex complex) (tagged 'complex add))
  (put 'sub '(complex complex) (tagged 'complex sub))
  (put 'mul '(complex complex) (tagged 'complex mul))
  (put 'div '(complex complex) (tagged 'complex div))
  (put 'equ? '(complex complex) equ?)
  (put '=zero? '(complex) =zero?)
  (put 'to-string '(complex) to-string)
  (put 'test 'complex-package test-package)

  'done)
