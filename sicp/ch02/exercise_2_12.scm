(load "../common/utils.scm")

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

; uncertainty(percentage) = width / center
; => width = center * uncertainty(percentage)
(define (make-center-percent c p)
  (make-center-width c (* c p)))

(define (percent x)
  (/ (width x)
     (center x)))

(define test-cp (make-center-percent 3.5 0.005))
(print-interval test-cp)
; [3.4825-3.5175]
(out (width test-cp)   ; ~= 0.0175
     (center test-cp)  ; ~= 3.5
     (percent test-cp) ; ~= 0.005
     )
