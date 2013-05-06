(load "../common/utils.scm")

; select iterative version of cont-frac
(define (cont-frac n d k)
  ; counter i from k to 1
  ; acc_next = N(i) / ( D(i) + acc ) 
  ; set acc to 0 initially
  (define (cont-frac-i i acc)
    (if (< i 1)
      acc
      (cont-frac-i (dec i) (/ (n i) (+ (d i) acc)))))
  (cont-frac-i k 0))

(define (tan-cf x k)
  ; multiple both side by (-x)
  ; => -x * tan(x) = -x^2 /(1 + (-x^2/( 3 + (...)  )))
  ; n(i) = - x*x
  ; d(i) = i*2 - 1
  (let ((neg-x-mul-tanx
          (cont-frac (const (- (* x x)))
                     (lambda (x) (- (* 2 x) 1))
                     k)))
    (/ neg-x-mul-tanx (- x))))

(define (test-tan-cf x)
  (let* ((lib-result (tan x))
         (tan-cf-result (tan-cf x 1000))
         (diff (abs (- lib-result tan-cf-result))))
    (display "test case: ")
    (display x)
    (newline)
    (display "tan: ")
    (display lib-result)
    (newline)
    (display "tan-cf: ")
    (display tan-cf-result)
    (newline)
    (display "difference: ")
    (display diff)
    (newline)))

; pick up some number and test the accuracy
(for-each
  test-tan-cf
  ; require an inexact number, the test case is integer from 1 to 10(rad)
  (list-in-range 1.0 10))
