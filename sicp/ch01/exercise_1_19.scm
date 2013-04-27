(load "../common/utils.scm")

(define (fib-fast n)
  (define (fib-iter a b p q count)
    (cond ((= count 0) b)
          ((even? count)
            (let ((p1 (+ (* p p) (* q q)))
                 (q1 (+ (* q q) (* 2 p q))))
             (fib-iter a b p1 q1 (/ count 2))))
          (else
            (let ((a1 (+ (* b q) (* a q) (* a p)))
                  (b1 (+ (* b p) (* a q))))
              (fib-iter a1 b1 p q (- count 1))))))
  (fib-iter 1 0 0 1 n))

(define (fib-linear n)
  (define (fib-iter a b count)
    (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))
  (fib-iter 1 ; fib 1 = 1
            0 ; fib 0 = 0
            n))

(out (fib-linear 20))
(out (fib-fast 20))

(define result-1 (car (time-test fib-linear 10000)))
(define result-2 (car (time-test fib-fast 10000)))

(out (= result-1 result-2))
; correctness test, should be #t
