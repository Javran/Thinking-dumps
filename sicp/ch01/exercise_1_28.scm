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
  (cond ; valid range: 1<x<n 
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
