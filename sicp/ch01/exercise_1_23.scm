(load "../common/utils.scm")

(define (next x)
  (if (= x 2)
    3
    (+ x 2)))

(out "test 'next' with init seed 2:")
(out (take-iterate next 2 5))
; should be (2 3 5 7 9)

(define (smallest-divisor n)
  (define (divides? a b)
    (= (remainder b a) 0))
  (define (square x) (* x x))
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n) ; impossible
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (next test-divisor)))))
  (find-divisor n 2))

(define (prime? n)
  (= n (smallest-divisor n)))

; return (#t/#f, time-elapsed)
(define (start-prime-test n start-time)
  (let* ((result (prime? n))
         (time-elapsed (- (runtime) start-time)))
    ; force evaluation order
    (cons result time-elapsed)))

; I'd like to make a quiet version.
(define (timed-prime-test n)
  (start-prime-test n (runtime)))

(out (timed-prime-test 100000007))

; here we need to keep track of computation performed
;     since the speed is too fast to infer duration
