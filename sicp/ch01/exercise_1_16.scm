(load "../common/utils.scm")

(define (my-expt b n)
  (define (my-expt-iter b n a)
    ; we need to keep a*(b^n) as constant
    (cond ((= n 0)
           ; a'*(b^n') = b^n , n' = 0 => a' = b^n 
           a)
          ((odd? n)
           ; a'*(b^n') = (a'*b)*b^(n'-1)
           (my-expt-iter b (- n 1) (* a b)))
          (else ; n is even
           ; a'*(b^n') = a'*(b^(n'/2))^2 = a'*(b^2)^(n'/2)
           (my-expt-iter (* b b) (/ n 2) a))))
  (my-expt-iter b n 1))

; the linear recursive version for the purpose of comparison
(define (my-expt-slow b n)
  (if (= n 0)
    1
    (* b (my-expt-slow b (- n 1)))))

(time-test my-expt-slow 3 10000)
(time-test my-expt 3 10000)

; equality test
(out (= (expt 3 10000) (my-expt 3 10000)))
