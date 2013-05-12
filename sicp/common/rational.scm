(define numer car)
(define denom cdr)

(define (make-rat n d)
  (if (= d 0)
    (error "denominator cannot be zero"))

  (let ((common (gcd n d)))
    ; now we're able to come up with the simplied version:
    (let ((n1 (/ n common))
          (d1 (/ d common)))
      (if (< d1 0)
        ; denom < 0, multiple both denom & numer by -1
        (cons (- n1) (- d1))
        ; else ... nothing to do
        (cons n1 d1)))))

; additive inverse
(define (neg-rat x)
  (make-rat (- (numer x)) (denom x)))

; multiplicative inverse
(define (inv-rat x)
  (make-rat (denom x) (numer x)))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (display-rat x)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

