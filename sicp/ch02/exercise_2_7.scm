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

(define x (make-interval 1 5))
(define y (make-interval 20 20.5))

(print-interval x)
; [1-5]
(print-interval y)
; [20-20.5]

(print-interval (add-interval x y))
; [21-25.5]

(print-interval (mul-interval x y))
; [20-102.5]

(print-interval (div-interval x y))
; mul [1-5] [1/20.5 - 1/20]
; mul [1-5] [0.04878 - 0.5]
; [0.04878 - 2,5]

(end-script)
