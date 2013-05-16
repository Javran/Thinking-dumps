(load "../common/utils.scm")

; copied from ex 2.8

(load "./1_4_interval_arithmetic.scm")

(define make-interval cons)
(define lower-bound car)
(define upper-bound cdr)

; pretty-print
(define (print-interval a)
  (display "[")
  (display (lower-bound a))
  (display "-")
  (display (upper-bound a))
  (display "]")
  (newline))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (spans-zero a)
  (let ((x (lower-bound a))
        (y (upper-bound a)))
    (cond ((= 0 x) #t)
          ((= 0 y) #t)
          ((< (* x y) 0) #t)
          (else #f))))

(define (div-interval x y)
  (if (spans-zero y)
    (error "divided by an interval that spans zero")
    (mul-interval
      x
      (make-interval (/ 1.0 (upper-bound y))
                     (/ 1.0 (lower-bound y))))))

(define iv1 (make-interval 90 110))
(define iv2 (make-interval 48 52))
(define iv3 (make-interval -10 10))

(print-interval (div-interval iv1 iv2))
(print-interval (div-interval iv1 iv3))
; will raise an error

(end-script)
