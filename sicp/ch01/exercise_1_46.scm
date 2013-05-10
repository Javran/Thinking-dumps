(load "../common/utils.scm")

; (good-enough? guess) => #t / #f
; (improve guess) => improve a guess
(define (iterative-improve good-enough? improve)
  (define (try guess)
    (if (good-enough? guess)
      guess
      (try (improve guess))))
  try)

(define (average-damping f)
  (lambda (x)
    (average x (f x))))

; sqrt: find y s.t. for a given x, y^2 = x => y = x/y
(define (my-sqrt x)
  (define (good-enough? y)
    (< (abs (- (square y) x))
       0.00001))
  (define (improve guess)
    ((average-damping (lambda (y) (/ x y))) guess))
  ((iterative-improve good-enough? improve) 1.0))

(out (my-sqrt (square 256)))
; ~= 256
(out (my-sqrt (square 1234)))
; ~= 1234

(end-script)
