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
  (let ((midpoint (average neg-point pos-point)))
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

; "search" is awkward to use directly,
;     because we might accidentally give it points
;     at which f's value do not have the required sign
; we can come up with a function that help us by calling "search" accordingly

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
            (search f a b))
          ((and (negative? b-value) (positive? a-value))
            (search f b a))
          (else (error "Values are not of opposite sign" a b)))))

(out (half-interval-method sin 2.0 4.0))
(out (half-interval-method sin 4.0 2.0))
; ~= pi

(let ((f (lambda (x) (- (* x x x) (* 2 x) 3))))
  (let ((result (half-interval-method f 1.0 2.0)))
    ; print and verify the result
    (out result (f result))))

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

(out (fixed-point cos 10.0))
; ~= 0.739 ; refer to: http://en.wikipedia.org/wiki/Fixed_point_%28mathematics%29

(out (fixed-point (lambda (y) (+ (sin y) (cos y))) 1.0))
; ~= 1.259

; "repeatedly improving a guess util the result satifies some criterion"

; we can define "sqrt" in terms of fixed-point search:
; finding the square root of x, requires finding a number y that x = y * y
; and x = y * y => y = x / y, we are looking for a fixed point of the function y = x/y

; the procedure below won't work, since y1 = x/y0, y2=x/y1=x/(x/y0)=y0
; (define (my-sqrt x)
;   (fixed-point (lambda (y) (/ x y)) 1.0))

; "the answer is always between our guess y and x/y"
; assume the answer is "yt", i.e. x = yt^2, yt>0
; case #1: if y < x / y
; => y^2 < x = yt^2
; => y < yt
; compare yt and x/y:
; yt - x/y = (yt * y - yt * yt)/y = yt/y * (y - yt) < 0
; => y < yt < x / y
; case #2: if y > x / y
; => y^2 > x = yt^2
; => y > yt
; compare yt and x/y:
; yt - x/y = yt/y * (y - yt) > 0
; => x / y < yt < y
; so yt is always between y and x/y

(define (my-sqrt x)
  (fixed-point (lambda (y) (average y (/ x y))) 1.0))

(out (my-sqrt 9))
; ~= 3

; despite "average damping" is not widely mentioned except in SICP exercises,
; here are some related links:
; * Why does average damping magically speed up the convergence of fixed-point calculators?
; http://stackoverflow.com/questions/3860929/why-does-average-damping-magically-speed-up-the-convergence-of-fixed-point-calcu
; * Bill the Lizard SICP Exercise 1.36 Fixed points and Average damping
; http://www.billthelizard.com/2010/07/sicp-exercise-136-fixed-points-and.html
 
; here's another series of problems concerning about y -> x / y, which does not converge since y2 = x/y1 = x/(x/y0) = yo
; * is there a general method of telling if some function is oscillate?
; * if it oscillates, does it really converges?
; * if it converges, how many generations will it take to converge?
; I think all of the questions above are connected to the correctness and efficiency of our algorithm
