(load "../common/utils.scm")

; miller-rabin test:
; * if 'a' raised to (n-1)-st power is congruent to 1 modulo n (s.t. 1<a<n)
;       then n is prime
; * use `expmod` to achieve a^(n-1), when squaring, check if we have discovered
;       non-trival square root of 1 modulo n
;       (i.e. for x =/= 0 or (n-1), x^2 mod n = 1
;       if any non-trival squart root is discovered, n is not a prime number
; * for at least half of the number a, 1<a<n, 
;       computing a^n will reveal a nontrival square root (if n is not a prime number)

; test if x is a non trival square root of 1 modulo n
(define (non-trival-square-root? x n)
  (cond
        ; ((or (<= x 1) (>= x n)) (error "number out of range"))
        ((= 1 x) #f) ; x should not be 1 nor n-1
        ((= (- n 1) x) #f)
        ((= 1 (remainder (square x) n)) #t)
        (else #f)))

; modified version of `expmod` (Miller-Rabin test)
; seems it'll be easier if we use recursive version of `expmod`
; returns a pair: (result, flag), flag will set to #t if a non-trival square root is discovered
(define (expmod base count m) ; base^n mod m
  (cond ((= count 0) 
         (cons 1 #f))
        ((even? count)
          (let ((sub-result (expmod base (/ count 2) m)))
            (cons
              (remainder (square (car sub-result)) m)
              (or (cdr sub-result) (non-trival-square-root? (car sub-result) count)))))
        (else
          (let ((sub-result (expmod base (- count 1) m)))
            (cons
              (remainder (* base (car sub-result)) m)
              (cdr sub-result))))))

(define (fermat-test n)
  (define (try-it a)
    (= (car (expmod a n n)) a))
  (try-it (random-range-in 1 (- n 1))))

(define (miller-rabin-test n)
  (define (try-it a)
    (let ((result (expmod a n n)))
      (and (= (car result) a)
           (not (cdr result)))))
  (try-it (random-range-in 1 (- n 1))))

(define (fast-prime? n times verifier)
  (cond ((= times 0) true)
        ((verifier n) (fast-prime? n (- times 1) verifier))
        (else #f)))

(define (verbose-prime-test n)
  (display "input: ")
  (display n)
  (newline)
  (display "output(fermat-test): ")
  (display (fast-prime? n 10 fermat-test))
  (newline)
  (display "output(miller-rabin-test): ")
  (display (fast-prime? n 10 miller-rabin-test))
  (newline))

; test some Carmichael numbers
(for-each verbose-prime-test
          '(561 1105 1729 2465 2821 6601))

; filter out all prime numbers less than 100:
(out (filter (lambda (x) (fast-prime? x 10 fermat-test))
             (list-in-range 2 99)))

(out (filter (lambda (x) (fast-prime? x 10 miller-rabin-test))
             (list-in-range 2 99)))

