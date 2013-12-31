(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./stream.scm")

(define pair-stream (pairs integers integers))

; since elements from `s` and `t` appear alternatively,
(print-few 20 pair-stream)

; let the number of pairs preceding pair `(x,y)` be `f(x,y)`
; f(1,1) = 0
; when x = 1, observe that
;   f(x,x+1) = f(x,x) + 1
;   f(x,y)   = f(x,y-1) + 2 (y > x+1)
; when x = 2, the sequence should be similar to that of x = 1
;   (2,2), (2,3), (3,3), (2,4), (3,4), ...
; but the reality is that every element get delayed by one
; when x = 3, the gap is also a constant: 3
; ...
; when x = n, let the gap be `g(n)`, we know that
;   g(1) = 0
;   g(n) = g(n-1) + g(n-1) + 1
; therefore, g(n) = 2^(n-1) - 1
; by observation, we know that:
;   f(1,1) = 0
;   f(x,y) = f(x-1,x) + g(x-1) + 1 (x   == y)
;   f(x,y) = f(x,x) + g(x) + 1     (x+1 == y)
;   f(x,y) = f(x,y-1) + 2g(x) + 2  otherwise  
(define (f x y)
  (define (g x)
    (- (expt 2 (- x 1)) 1))
  (cond 
    ((= x y 1) 0)
    ((= x y)
     (+ (f (- x 1) x) (g (- x 1)) 1))
        ((= (+ x 1) y)
          (+ (f x x) (g x) 1))
        (else
          (+ (f x (- y 1))
             (* 2 (g x))
             2))))

; make tests
(out (head (drop (f 10 35) pair-stream)))
; (10,35)
(out (head (drop (f 11 26) pair-stream)))
; (11,26)

(out (f 1 100)
     (f 99 100)
     (f 100 100))

(end-script)
