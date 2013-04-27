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
