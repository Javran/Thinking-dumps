(load "../common/utils.scm")

; S = h/3 * (y(0) + 4*y(1) + 2*y(2) + 4*y(3) + 2*y(4) + ... + 2*y(n-2) + 4*y(n-1) + y(n))
; let's define an auxiliary function t(x):
; t(x) = 4 * y(x*2-1) + 2 * y(x*2)
; t(1) = 4*y(1) + 2*y(2)
; t(2) = 4*y(3) + 2*y(4)
; ...
; S = h/3 * (y(0) + t(1) + t(2) + ... + t(m) - y(n))
;     where m * 2 - 1 = n - 1 => m = n/2
; verification:
; S = h/3 * (y(0) + (4*y(1) + 2*y(2)) + (4*y(3) + 2*y(4)) + ... + (4*y(n-1) + 2*y(n)) - y(n))
;
; so we have:
; S = h/3 * (y(0) - y(n) + sum( t(i), i = 1 .. m ) )

(define (sum a b f next)
  (if (> a b)
    0
    (+ (f a)
       (sum (next a) b f next))))

; verification
(out (sum 1 100 (lambda (x) x) (lambda (x) (+ x 1))))
; should be 5050

(define (integral f a b dx)
  (define (add-dx x)
    (+ x dx))
  (* (sum (+ a (/ dx 2.0)) b f add-dx)
     dx))

(define (integral-sr f a b n)
  (define h
    (/ (- b a) n))
  (define (y k)
    (f (+ a (* k h))))
  (define (t x)
    (+ (* 4 (y (- (* x 2) 1)))
       (* 2 (y (* x 2)))))
  (define sum_t
    (sum 1 (/ n 2) t (lambda (x) (+ x 1))))
  (* (/ h 3)
     (+ (- (y 0)
           (y n))
        sum_t)))

(out "integral (normal):"
     (integral cube 0 1 0.01)
     (integral cube 0 1 0.001)
     "integral (Simpson's Rule):"
     (integral-sr cube 0 1 100)
     (integral-sr cube 0 1 1000))
; come up with exactly 1/4 using Simpson's Rule ?!

; calculate int(sin(x), x = 0 -> 0.12345)
; the result should roughly be:
; 0.0076102788885118238871 (or 7.6102788885118238871e-3) 

(out 
     "integral (normal):"
     (integral sin 0 0.12345 0.0001)
     (integral sin 0 0.12345 0.00001)
     "integral (Simpson's Rule):"
     (integral-sr sin 0 0.12345 100)
     (integral-sr sin 0 0.12345 1000))
