(load "../common/utils.scm")

(define (my-expt b n)
  (if (= n 0)
    1
    (* b (my-expt b (- n 1)))))

(out (my-expt 2 10))
; 1024

; space: theta( n ); time: theta( n )

(define (my-expt-2 b n)
  (define (my-expt-iter b counter product)
    (if (= 0 counter)
      product
      (my-expt-iter b (- counter 1) (* b product))))
  (my-expt-iter b n 1))

(out (my-expt-2 2 10))
; 1024
; space: theta( 1 ); time: theta( n )

; we can make the computation faster by ...
(define (fast-expt b n)
  (define (square x) (* x x))
  (define (my-even? x) (= 0 (remainder x 2)))
  (cond ((= n 0) 1)
        ((my-even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))
(out (fast-expt 2 10))
; 1024

