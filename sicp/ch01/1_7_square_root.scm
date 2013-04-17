(load "../common/utils.scm")

; how to produce an approximate square root of 2
; for example: if we guess y1*y1 = x, by calculating y2 = (y1+(x/y1))/2, 
;     we'll have a better y2 that y2*y2 is closer to x than y1*y1 is

; refer to: Newton's method
; http://en.wikipedia.org/wiki/Newton%27s_method
; we need (* guess guess) to be (sqrt x)
; the result is: x(n+1) = (guess + x(n)/guess)/2

(define (square x) (* x x))

(define (average x y)
  (/ (+ x y) 2))

; given a guess, how to improve it?
(define (improve guess x)
  ; average of guess & (x/guess)
  (average guess (/ x guess)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x)
               x)))

(define (my-sqrt x) (sqrt-iter 1.0 x))

(out
  (my-sqrt 100)
  ; ~ 10
  (my-sqrt 200)
  ; ~ 14.1
  (my-sqrt 0.25)
  ; ~ 0.5 

  (my-sqrt 9)
  ; ~ 3
  (my-sqrt (+ 100 37))
  ; ~ 11.7
  (square (my-sqrt 1000))
  ; ~ 1000
  )
