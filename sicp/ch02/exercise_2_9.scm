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

(define iv1 (make-interval 3.9 4.1)) ; 0.1
(define iv2 (make-interval 0.8 1.2)) ; 0.2
(define iv3 (make-interval 100 120)) ; 10

(out 
  (width-interval iv1)
  ; ~= 0.1
  (width-interval iv2)
  ; ~= 0.2
  (width-interval (add-interval iv1 iv2))
  ; ~= 0.3
  (width-interval (sub-interval iv1 iv2))
  ; ~= 0.3
  (width-interval (add-interval iv3 iv2))
  ; = 10 + 0.2 ~= 10.2 ; the range expands
  (width-interval (sub-interval iv3 iv1))
  ; = 10 + 0.1 ~= 10.1
  ""
  (width-interval (mul-interval iv1 iv2))
  (width-interval (div-interval iv1 iv2))
  (width-interval (mul-interval iv1 iv3))
  (width-interval (div-interval iv2 iv3))
  )

(end-script)
