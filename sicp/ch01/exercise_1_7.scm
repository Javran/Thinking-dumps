(load "../common/utils.scm")

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

; an example for "good-enough?" is inadequate for small numbers
(out
  (my-sqrt 0.0001)
  ; 0.03 <- might considered a big error
  (sqrt 0.0001)
  ; 0.01
  )

; an example for "good-enough?" is inadequate for big numbers
; actually I don't know why it is inadequate ...
; the point might be: it's already precise enough for big numbers,
;     but the fixed precision cannot satisfiy both small numbers and big numbers
(out
  (my-sqrt 100000)
  ; 316.2
  (sqrt 100000)
  ; 316.2
  )
