(define (tagged tag f)
  (lambda args
    (attach-tag tag (apply f args))))

(define (install-integer-package)
  (define (make-integer x)
    (if (integer? x)
      (attach-tag 'integer x)
      (error "MAKE-INTEGER: not an integer" x)))

  (define (integer->rational x)
    ((get 'make 'rational) (contents x) 1))
  (put 'make 'integer make-integer)
  (put 'add '(integer integer) (tagged 'integer +))
  (put 'sub '(integer integer) (tagged 'integer -))

  (put-coercion 'integer 'rational integer->rational)
  'done)

(define (install-rational-package)
  (define numer car)
  (define denom cdr)
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g)
            (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (neg-rat x)
    (make-rat (- (numer x))
              (denom x)))
  (define (sub-rat x y)
    (add-rat x (neg-rat y)))

  (define (rat-only x y)
    (out "this is a rational only procedure for test")
    (out x y))

  (put 'make 'rational (tagged 'rational make-rat))
  (put 'add '(rational rational) (tagged 'rational add-rat))
  (put 'sub '(rational rational) (tagged 'rational sub-rat))
  (put 'rat-only '(rational rational) rat-only)
  'done)

(define (install-real-package)
  (put 'make 'real (tagged 'real identity))
  (put 'add '(real real) (tagged 'real +))
  (put 'sub '(real real) (tagged 'real -))
  'done)

(define (install-complex-package)
  (define make-complex cons)
  (define real-part car)
  (define imag-part cdr)

  (define (add a b)
    (make-complex
      (+ (real-part a) (real-part b))
      (+ (imag-part a) (imag-part b))))

  (define (neg x)
    (make-complex (- (real-part x))
                  (- (imag-part x))))

  (define (sub a b)
    (add a (neg b)))

  (put 'make 'complex (tagged 'complex make-complex))
  (put 'add '(complex complex) (tagged 'complex add))
  (put 'sub '(complex complex) (tagged 'complex sub))
  'done)
