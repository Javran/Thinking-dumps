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

(define (percent x)
  (/ (width x)
     (center x)))

; start my solution here
; assume all numbers are positive:
; iv1*iv2 = [lb1*lb2, ub1*ub2]
; p = w/c
;
; lb1 = c1 - w1 = c1 - c1 * p1
;     = c1*(1-p1)
; ub1 = c1*(1+p1)
; lb2 = c2*(1-p2)
; ub2 = c2*(1+p2)
;
; lb1*lb2 = c1*c2*(1-p1)*(1-p2)
; ub1*ub2 = c1*c2*(1+p1)*(1+p2)
; c_result = c1*c2 * ( (1+p1)*(1+p2) + (1-p1)*(1-p2) )/2
; w_result = c1*c2 * ( (1+p1)*(1+p2) - (1-p1)*(1-p2) )/2
; p_result = ( (1+p1)*(1+p2) - (1-p1)*(1-p2) ) / ( (1+p1)*(1+p2) + (1-p1)*(1-p2) )
; p_result = (p1+p2) / (1+p1*p2)
; the condition "small percentage tolerances" have not yet been used ... something missed?
