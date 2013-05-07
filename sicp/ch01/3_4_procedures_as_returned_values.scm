(load "../common/utils.scm")

; a procedure that returns functions / procedures
(define (average-damping f)
  (lambda (x)
    (mid x (f x))))

(out ((average-damping square) 10))
; average of 10 and 100 is 55

; move procedures from part 1.3.3
(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
        next
        (try next))))
  (try first-guess))


(define (my-sqrt x)
  ; find fixed point of function y -> x/y using average damping
  (fixed-point (average-damping (lambda (y) (/ x y)))
               1.0))

(out (sqrt 123))
(out (my-sqrt 123))

; we can also find cube root for a given x:
; y^3 = x, y -> x / y^2

(define (cube-root x)
  (fixed-point (average-damping (lambda (y) (/ x (* y y))))
               1.0))

; compare two results below
(out (expt 1234567 (/ 1 3)))
(out (cube-root 1234567))
