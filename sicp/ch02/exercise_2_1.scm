(load "../common/utils.scm")

(define numer car)
(define denom cdr)

(define (display-rat x)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define (make-rat n d)
  (if (= d 0)
    (error "denominator cannot be zero"))

  (let ((common (gcd n d)))
    ; now we're able to come up with the simplied version:
    (let ((n1 (/ n common))
          (d1 (/ d common)))
      (if (< d1 0)
        ; denom < 0, multiple both demon & numer by -1
        (cons (- n1) (- d1))
        ; else ... nothing to do
        (cons n1 d1)))))

(display-rat (make-rat (apply * (list-in-range 5 10))
                       (apply * (list-in-range 1 10)))) ; 1/24
(newline)

(display-rat (make-rat -2 -3)) ; 2/3
(newline)

(display-rat (make-rat 65536 -4294967296)) ; -1/65536
(newline)

(display-rat (make-rat -1 10)) ; -1/10
(newline)
