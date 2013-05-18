(load "../common/utils.scm")

(load "./1_4_interval_arithmetic.scm")

(define make-interval cons)
(define lower-bound car)
(define upper-bound cdr)

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center x)
  (average (lower-bound x)
           (upper-bound x)))

(define (width x)
  (/ (- (upper-bound x)
        (lower-bound x))
     2))

(print-interval (make-center-width 3.5 0.15)) ; [3.35-3.65]


