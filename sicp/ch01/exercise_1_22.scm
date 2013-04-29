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

(define (timed-prime-test n)
  (newline)
  (display n) 
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (prime? n)
    ; some modification: return the prime number or #f
    (begin
      (report-prime (- (runtime) start-time))
      n)
    (begin
      (newline)
      #f)))
  
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time)
  (newline))

(let ((result (timed-prime-test 2147483647)))
  ; well ... seems (runtime) is not an integer in mit-scheme ...
  (display "the prime is ")
  (display result)
  (newline))

; will return a list of primes in range
(define (search-for-primes start-num end-num prime-count)
  ; if end-num is #f, there is no upbound
  (if (or (and end-num (> start-num end-num)) (= prime-count 0)) ; search is done
    '()
    (if (odd? start-num)
      (let ((result (timed-prime-test start-num)))
            ; (rest-results (search-for-primes (+ start-num 2) end-num)))
        (if result ; force the evaluation order: first 'result' then 'rest-results'
          (cons result (search-for-primes (+ start-num 2) end-num (- prime-count 1)))
          (search-for-primes (+ start-num 2) end-num prime-count)))
      ; else
      (search-for-primes (+ start-num 1) end-num prime-count))))

; find up to 5 primes within range [10,100]
(out (search-for-primes 10 100 5))

(begin
  (out (search-for-primes   1000 #f 3))
  ; (1009 1013 1019)
  (out (search-for-primes  10000 #f 3))
  ; (10007 10009 10037)
  (out (search-for-primes 100000 #f 3))
  ; (100003 100019 100043) 
  )

; (runtime) is too small to come up with a conclusion 
; we need more test ...
(search-for-primes 100000000 #f 1)
(search-for-primes 1000000000 #f 1)
(search-for-primes 10000000000 #f 1)
(search-for-primes 100000000000 #f 1)

; put some results here:
; 100000007 *** .03999999999999998
; 1000000007 *** .14999999999999997
; 10000000019 *** .4800000000000001
; 100000000003 *** 1.5799999999999998
; f(n) = sqrt(n) * c
; c = f(n) / sqrt(n) should be relatively stable
; (sqrt 100000007) / 0.03999999999999998   = 250000.00874999995
; (sqrt 1000000007) / 0.14999999999999997  = 210818.51141575677
; (sqrt 10000000019) / 0.4800000000000001  = 208333.33353124995
; (sqrt 100000000003) / 1.5599999999999998 = 202710.10642409063

; 0.03999999999999998 / sqrt 100000007    = 3.999999860000006e-6
; 0.14999999999999997 / sqrt 1000000007   = 4.74341647365061e-6
; 0.4800000000000001  / sqrt 10000000019  = 4.799999995440001e-6
; 1.5599999999999998  / sqrt 100000000003 = 4.933153149788674e-6

; I think the result is relative compatible with the notion
; as the increase of 'n', the result will become preciser.
