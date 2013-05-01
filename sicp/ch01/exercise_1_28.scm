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

(define (expmod-i base count m)
  (define (expmod-iter base count m n) ; constant: (base^count * n) mod m
    (cond ((= count 0) (remainder n m)) ; count = 0, (base^count * n) mod m = n mod m
          ((even? count)
            ; (base^count * n) mod m = ((base^2)^(count/2) * n) mod m
            (expmod-iter (remainder (square base) m) (/ count 2) m n))
          (else
            (expmod-iter base (- count 1) m (remainder (* base n) m)))))
  (expmod-iter base count m 1))
