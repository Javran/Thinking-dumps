(load "../common/utils.scm")

(define phi (/ (+ (sqrt 5) 1) 2))

; (sqrt 5 + 1)/2 is a root of equation x*x - x - 1 = 0
(out (+ (* phi phi) (- phi) (- 1)))

; x*x - x - 1 = 0
; => x - 1 - 1/x = 0
; => x = 1 + 1/x 

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

(out phi
     (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)
     (fixed-point (lambda (x) (average (+ 1 (/ 1 x)) x)) 1.0)) ; avarage damping
