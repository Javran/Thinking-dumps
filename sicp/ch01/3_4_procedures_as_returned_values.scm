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

; Newton's method
; the idea is: next(x) = x - g(x) / dg(x)
;     where dg is derivative of g at point (x,f(x))

(define dx 0.00001)

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x)) dx)))

; f(x) = x^3 => df(x) = 3*x^2, test point at (3, f(3))
(out ((deriv cube) 3))
; should be 3*3^2 = 27
(out ((deriv cube) 5))
; should be 3*5^2 = 75

; transform a function f(x) to its next(x) form
(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (sqrt-nm x)
  (newtons-method
    (lambda (y) (- (* y y) x))
    1.0))

(out (sqrt 1111))
(out (sqrt-nm 1111))

; find a root of f(x) = (x-1.2345)(x+10)(x+20)
(let ((f (lambda (x) (* (- x 1.2345) (+ x 10) (+ x 20)))))
  (out (newtons-method f 2))
  ; ~= 1.2345
  (out (newtons-method f (- 100)))
  ; ~= -20
  (out (newtons-method f (- 8)))
  ; ~= -10
  )

; abstractions and first-class procedures
(define (fixed-point-of-transform g transform guess)
  ; actually I don't think this function is necessary ...
  ; as this version just flattens the parameters to "fixed-point"
  (fixed-point (transform g) guess))

(define (sqrt-ad x)
  (fixed-point-of-transform
    (lambda (y) (/ x y))
    average-damping
    1.0))

(define (sqrt-nm-2 x)
  (fixed-point-of-transform
    (lambda (y) (- (* y y) x))
    newton-transform
    1.0))

(out (sqrt 4321))
(out (sqrt-ad 4321))
(out (sqrt-nm-2 4321))
