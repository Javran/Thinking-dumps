(load "../common/utils.scm")

; copied from ex 2.7

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

; the lower-bound `x-y` can reach is to subtract lower-bound of x by y's upper bound
; the upper-bound `x-y` can reach is to subtract upper-bound of x by y's lower-bound
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

(define x (make-interval 2.5 3.5))
(define y (make-interval 0.8 1.2))

(print-interval (sub-interval x y))
; [1.3 - 2.7]

(end-script)
