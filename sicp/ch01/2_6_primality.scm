(load "../common/utils.scm")

(define (smallest-divisor n)
  (define (divides? a b)
    (= (remainder b a) 0))
  (define (square x) (* x x))
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n) ; impossible
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (+ test-divisor 1)))))
  (find-divisor n 2))

(define (prime? n)
  (= n (smallest-divisor n)))

(define testcase
  '(2 3 4 5 6 7 8 9 10 11 12 13))

(out "smallest-divisor table")
(for-each 
  (lambda (x)
    (let ((result (smallest-divisor x)))
      (display x)
      (display "\t")
      (display result)
      (newline)))
  testcase)

(out "primes less than 14:")
(out (filter prime? testcase))

(define (expmod base count m) ; base^n mod m
  (cond ((= count 0) 1)
        ((even? count)
          (remainder (square (expmod base (/ count 2) m)) m))
        (else
          (remainder (* base (expmod base (- count 1) m)) m))))

(out (time-test expmod 23 4567 8901))
; (8717 . <time>)

; challenge: iterative expmod?
(define (expmod-i base count m)
  (define (expmod-iter base count m n) ; constant: (base^count * n) mod m
    (cond ((= count 0) (remainder n m)) ; count = 0, (base^count * n) mod m = n mod m
          ((even? count)
            ; (base^count * n) mod m = ((base^2)^(count/2) * n) mod m
            (expmod-iter (square base) (/ count 2) m n))
          (else
            (expmod-iter base (- count 1) m (remainder (* base n) m)))))
  (expmod-iter base count m 1))

(out (time-test expmod 23 4567 8901))
; (8717 . <time>)
