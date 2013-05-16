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

; the lower-bound `x-y` can reach is to subtract lower-bound of x by y's upper bound
; the upper-bound `x-y` can reach is to subtract upper-bound of x by y's lower-bound
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

(define (width-interval x)
  (/ (- (upper-bound x)
        (lower-bound x))
     2.0))

(define iv1 (make-interval 3.9 4.1))
(define iv2 (make-interval 0.8 1.2))

(define add-res (add-interval iv1 iv2))
(define sub-res (sub-interval iv1 iv2))

(out 
  (width-interval iv1)
  ; ~= 0.1
  (width-interval iv2)
  ; ~= 0.2
  (width-interval add-res)
  ; ~= 0.3
  (width-interval sub-res)
  ; ~= 0.3
  )


(end-script)
