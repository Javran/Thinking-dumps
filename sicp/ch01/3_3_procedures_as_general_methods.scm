(load "../common/utils.scm")

; half interval method: finding root of a equation f(x) = 0
; * f is continuous function of x
; * given a,b s.t. f(a) < 0 < f(b), f must have a zero between a and b
; * let x be the average of a & b, compute f(x)
; * if f(x) > 0 => f must have a zero between a and x
; * if f(x) < 0 => f must have a zero between x and b
; * if f(x) = 0 (or f(x) is close enough to 0) => x is a root
; * the interval of the uncertainty is reduced by half at each step

(define (search f neg-point pos-point)
  (let ((midpoint (mid neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
      midpoint
      (let ((test-value (f midpoint)))
        (cond ((positive? test-value)
                (search f neg-point midpoint))
              ((negative? test-value)
                (search f midpoint pos-point))
              ; midpoint happen to be 0
              (else midpoint))))))

(define (close-enough? x y)
  (< (abs (- x y)) 0.001))

(let ((test-f (lambda (x) (* (- x 1) (+ x 2))))
      (neg-point 0.0)
      (pos-point 100.0))
  ; test finding root of f(x) = (x-1)*(x+2) in range (0..100)
  (out (search test-f neg-point pos-point)))
; the output should roughly be 1.0
