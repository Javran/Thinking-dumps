(load "../common/utils.scm")

; actually I hate the parameter arrangemet here,
;     I would prefer "sum term next a b" ...
(define (sum term a next b)
  (if (> a b)
    0
    (+ (term a) (sum term (next a) next b))))
  
(define (pi-sum a b)
  (sum (lambda (x) (/ 1.0 (* x (+ x 2))))
       a
       (lambda (x) (+ x 4))
       b))

(out (pi-sum 1 1000))

(define (integral f a b dx)
  (* (sum f
          (+ a (/ dx 2.0))
          (lambda (x) (+ x dx))
          b)
     dx))

(out (integral cube 0 1 0.0001))
; ~= 0.25

; a lambda can be used any where a procedure can be used:
(out ((lambda (x y z) (+ x y (square z))) 1 2 3))
; 12
