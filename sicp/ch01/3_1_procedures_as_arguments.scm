(load "../common/utils.scm")

(define (sum-integers a b)
  (if (> a b)
    0
    (+ a (sum-integers (+ a 1) b))))

(out (sum-integers 1 100))
; 5050

(define (sum-cubes a b)
  (if (> a b)
    0
    (+ (cube a)
       (sum-cubes (+ a 1) b))))

(out (sum-cubes 1 20))
; 44100

(define (pi-sum a b)
  (if (> a b)
    0
    (+ (/ 1.0 (* a (+ a 2)))
       (pi-sum (+ a 4) b))))

(out (* 8 (pi-sum 1 1000)))
; converges to pi

; 3 functions above share a common pattern
; we can make slots to make our sum-up function more general
(define (sum term a next b)
  (if (> a b)
    0
    (+ (term a)
       (sum term (next a) next b))))

; now we can define sum-integers, sum-cubes and pi-sum in terms of 'sum' function:
(define (identity x) x)
(define (inc x) (+ x 1))

(define (sum-integers-2 a b)
  (sum identity a inc b))

(define (sum-cubes-2 a b)
  (sum cube a inc b))

(define (pi-sum-2 a b)
  (define (term x)
    (/ 1.0 (* x (+ x 2))))
  (define (plus-4 x)
    (+ x 4))
  (sum term a plus-4 b))

(out (sum-integers-2 1 100))
; 5050

(out (sum-cubes-2 1 20))
; 44100

(out (* 8 (pi-sum-2 1 1000)))
; ~= pi

(define (integral f a b dx)
  (define (add-dx x)
    (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

; I think this would work as well ...
(define (integral-2 f a b dx)
  (define (add-dx x)
    (+ x dx))
  (* (sum f a add-dx b)
     dx))

(out 
  (integral   cube 0 1 0.01)
  (integral-2 cube 0 1 0.01)
  (integral   cube 0 1 0.001)
  (integral-2 cube 0 1 0.001)
  (integral   cube 0 1 0.0001)
  (integral-2 cube 0 1 0.0001))
; ~= 0.5
