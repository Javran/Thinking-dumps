(load "../common/utils.scm")

(define (my-mul a b)
  (if (= b 0)
    0
    (+ a (my-mul a (- b 1)))))

; correctness verification
(out (* 123 456))
(out (my-mul 123 456))

; functions that we can utilize
(define (double x) (+ x x))
(define (halve x)
  (if (odd? x)
    (error "x is odd")
    (/ x 2)))

(define (fast-mul a b)
  (cond ((= b 0) 0)
        ((odd? a) (+ a (fast-mul a (- b 1))))
        (else (double (fast-mul (halve a) b)))))

(out (fast-mul 123 456))

(time-test my-mul 12345 39999)
(time-test fast-mul 12345 39999)
; why the heck is fast-mul so slow???
