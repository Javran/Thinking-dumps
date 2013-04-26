(load "../common/utils.scm")

; functions that we can utilize
(define (double x) (+ x x))
(define (halve x)
  (if (odd? x)
    (error "x is odd")
    (/ x 2)))

(define (my-mul a b)
  (define (my-mul-iter a b n) ; let's make (n + a*b) a constant
    (cond ((= b 0) n) ; if b=0, a*b=0, n = result
          ((odd? b) (my-mul-iter a (- b 1) (+ n a))) ; if b is odd, n+a*b = (n+a)+a*(b-1)
          (else (my-mul-iter (double a) (halve b) n)))) ; if b is even, n+a*b = n+(2*a)*(b/2)
  (my-mul-iter a b 0))

(define (mul a b)
  (if (= b 0)
    0
    (+ a (mul a (- b 1)))))

(out (mul 1234 5678))
(out (my-mul 1234 5678))
(out (* 1234 5678))

(time-test mul 123 34567)
(time-test my-mul 123 34567)
(time-test * 123 34567)
