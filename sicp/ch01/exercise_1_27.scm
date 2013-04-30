(load "../common/utils.scm")

; challenge: iterative expmod?
(define (expmod-i base count m)
  (define (expmod-iter base count m n) ; constant: (base^count * n) mod m
    (cond ((= count 0) (remainder n m)) ; count = 0, (base^count * n) mod m = n mod m
          ((even? count)
            ; (base^count * n) mod m = ((base^2)^(count/2) * n) mod m
            (expmod-iter (remainder (square base) m) (/ count 2) m n))
          (else
            (expmod-iter base (- count 1) m (remainder (* base n) m)))))
  (expmod-iter base count m 1))

; returns a pair: (#f . x) if the test fails when a = x
; elsewise, (#t . #t) is returned
(define (fermat-test-all n)
  (define (try-it a)
    (= (expmod-i a n n) a))
  (let loop ((i 1))
    (if (< i n)
      (if (try-it i)
        ; keep going
        (loop (+ i 1))
        ; else
        (cons #f i)) ; failed the test when a = i
      ; else
      (cons #t #t))))

; safe prime test
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

(define (verbose-fermat-test-all n)
  (display "input: ")
  (display n)
  (newline)
  (display "prime? ")
  (display (prime? n))
  (newline)
  (let ((result (fermat-test-all n)))
    (if (car result)
      (begin
        (display "test passed")
        (newline))
      (begin
        (display "test failed when a = ")
        (display (cdr result))
        (newline)))))

(verbose-fermat-test-all 10)
; will fail when a = 2

; test all Carmichael numbers listed in the book
(for-each verbose-fermat-test-all
          '(561 1105 1729 2465 2821 6601))
; fermat-test are passed for each of them, but none of them is prime number
