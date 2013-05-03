(load "../common/utils.scm")

; product - recursive version
(define (product-rec f next a b)
  (if (> a b)
    1
    (* (f a) (product-rec f next (next a) b))))

; product - iterative version
(define (product-itr f next a b)
  (define (product-iter a acc)
    (if (> a b)
      acc
      (product-iter (next a) (* (f a) acc))))
  (product-iter a 1))

; define factorial function in terms of product function
(define (make-factorial product x)
  (product identity inc 1 x))

; test factorial function making use of recursive & iteractive version
(out (make-factorial product-rec 10))
(out (make-factorial product-itr 10))
; 3628800

; compute the approximations of pi

; pi/4 = (2*4)/(3*3) * (4*6)/(5*5) * (6*8)/(7*7) ...
; t(x) = ((x*2)*(x*2+2)) / ((x*2+1)*(x*2+1))
; t(1) = (2*4)/(3*3)
; t(2) = (4*6)/(5*5)
; ...

(define (calc-pi n)
  (define (t x)
    (let ((2x (* x 2)))
      (exact->inexact
        (/ (* 2x (+ 2x 2))
           (* (+ 2x 1) (+ 2x 1))))))
    (* (product-itr t inc 1 n)
       4))

(apply out (map calc-pi
                '(100
                  1000
                  10000)))
; close to 3.1415 ... 
