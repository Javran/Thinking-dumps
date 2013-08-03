(define (install-complex2-package)
  (define (valid-type t)
    (cond ((equal? t 'complex) #f)
          ((equal? t 'complex2) #f)
          (else #t)))

  ;; constructors
  ; when we got something wrong, I'll just return #f
  ;   instead of ending up with error
  (define (make-from-real-imag r i)
    (if (and (valid-type (type-tag r))
             (valid-type (type-tag i)))
      ((get 'make-from-real-imag 'rect) r i)
      #f))

  (define (make-from-mag-ang m a)
    (if (and (valid-type (type-tag m))
             (valid-type (type-tag a)))
      ((get 'make-from-mag-ang 'polar) m a)
      #f))

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
      (apply-generic 'add (real-part x) (real-part y))
      (apply-generic 'add (imag-part x) (imag-part y))))

  (define (sub x y)
    (make-from-real-imag
      (apply-generic 'sub (real-part x) (real-part y))
      (apply-generic 'sub (imag-part x) (imag-part y))))

  (define make-real (get 'make 'real))
  (define (make-num x) (drop (make-real x)))

  (define (mul x y)
    (make-from-mag-ang (make-num ((unwrapped *) (magnitude x) (magnitude y)))
                       (make-num ((unwrapped +) (angle x) (angle y)))))

  (define (equ? x y)
    (=zero? (sub x y)))

  (define (=zero? x)
    (define (small-enough x)
      (< (abs x) eps))
    ((unwrapped small-enough) (magnitude x)))

  ;; coercion system
  ; (N/A)

  ;; visualization
  (define (print-num x)
    (define (print-num-g x)
      (apply-unary 'print-num x))
    (display "complex2: ")
    (display "[real=")
    (print-num-g (real-part x))
    (display ", imag=")
    (print-num-g (imag-part x))
    (display "]"))

  (load "./exercise_2_86_pkg_rect.scm")
  (load "./exercise_2_86_pkg_polar.scm")
  (install-rect-package)
  (install-polar-package)

  (put 'make-from-real-imag 'complex2 (tagged 'complex2 make-from-real-imag))
  (put 'make-from-mag-ang 'complex2 (tagged 'complex2 make-from-mag-ang))
  (put 'real-part 'complex2 real-part)
  (put 'imag-part 'complex2 imag-part)
  (put 'magnitude 'complex2 magnitude)
  (put 'angle 'complex2 angle)
  (put 'add '(complex2 complex2) (tagged 'complex2 add))
  (put 'sub '(complex2 complex2) (tagged 'complex2 sub))
  (put 'mul '(complex2 complex2) (tagged 'complex2 mul))
  (put 'equ? '(complex2 complex2) equ?)
  (put '=zero? 'complex2 =zero?)
  (put 'project 'complex2 project)
  (put 'print-num 'complex2 print-num)
  'done)
