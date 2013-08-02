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
  (define (project x) ((get 'make 'real) (real-part x)))

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
