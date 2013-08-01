(define (install-complex-package)
  ;; constructors
  (define (make-from-real-imag r i)
    ((get 'make-from-real-imag 'rect) r i))
  (define (make-from-mag-ang m a)
    ((get 'make-from-mag-ang 'polar) m a))

  ;; accessors
  (define (real-part x)
    (apply-generic 'real-part x))
  (define (imag-part x)
    (apply-generic 'imag-part x))
  (define (magnitude x)
    (apply-generic 'magnitude x))
  (define (angle x)
    (apply-generic 'angle x))

  ;; math operations
  (define (add x y)
    (make-from-real-imag
      (+ (real-part x) (real-part y))
      (+ (imag-part x) (imag-part y))))
  (define (inv x)
    (make-from-real-imag
      (- (real-part x))
      (- (imag-part x))))
  (define (sub x y)
    (add x (inv y)))

  (define (mul x y)
    (make-from-mag-ang (* (magnitude x) (magnitude y))
                       (+ (angle x) (angle y))))

  (define (equ? x y)
    (=zero? (sub x y)))

  (define (=zero? x)
    (< (magnitude x) eps))

  ;; coercion system
  (define project real-part)

  ;; visualization
  (define (print-num x)
    (display "complex: ")
    (display (real-part x))
    (display " + ")
    (display (imag-part x))
    (display "i"))

  (load "./4_3_data_directed_aly_impl.scm")
  (load "./4_3_data_directed_ben_impl.scm")
  (install-rect-package)
  (install-polar-package)

  (put 'make-from-real-imag 'complex (tagged 'complex make-from-real-imag))
  (put 'make-from-mag-ang 'complex (tagged 'complex make-from-mag-ang))
  (put 'real-part 'complex real-part)
  (put 'imag-part 'complex imag-part)
  (put 'magnitude 'complex magnitude)
  (put 'angle 'complex angle)
  (put 'add '(complex complex) (tagged 'complex add))
  (put 'sub '(complex complex) (tagged 'complex sub))
  (put 'mul '(complex complex) (tagged 'complex mul))
  (put 'equ? '(complex complex) equ?)
  (put '=zero? 'complex =zero?)
  (put 'project 'complex project)
  (put 'print-num 'complex print-num)
  'done)

(define (test-complex-package)
  (display "Testing complex package #1 ")
  (let* ((make-ri (get 'make-from-real-imag 'complex))
         (make-ma (get 'make-from-mag-ang 'complex))
         (a (make-ri (sqrt 3) 0))
         (b (make-ri 0 1))
         (c (make-ma 2 (/ pi 6))))
    (let ((testcases (list
                       (cons (list 'add b b)
                             (make-ri 0 2))
                       (cons (list 'sub a b)
                             (make-ri (sqrt 3) -1))))
          (f (lambda args
               (apply apply-generic args))))
      (do-test f testcases))
    (display "Testing complex package #2 ")
    (define (assert x)
      (if x
        (display ".")
        (error "Assertion failed")))
    (define (add x y) (apply-generic 'add x y))
    (define (sub x y) (apply-generic 'sub x y))
    (define (mul x y) (apply-generic 'mul x y))
    (define (equ? x y) (apply-generic 'equ? x y))
    (define (=zero? x) (apply-unary '=zero? x))
    (define (imag-part x) (apply-unary 'imag-part x))
    (define (magnitude x) (apply-unary 'magnitude x))

    (assert (equ? (add a b) c))
    (assert (equ? (mul a b) (make-ri 0 (sqrt 3))))
    (assert (< (abs (- (imag-part c) 1)) eps))
    (assert (< (abs (- (magnitude c) 2)) eps))
    (out "Test passed.")
    ))
