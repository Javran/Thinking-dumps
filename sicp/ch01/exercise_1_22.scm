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
  (if (or (> start-num end-num) (= prime-count 0)) ; search is done
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

(let ((search-up-bound 2147483647))
  (out (search-for-primes   1000 search-up-bound 3))
  ; (1009 1013 1019)
  (out (search-for-primes  10000 search-up-bound 3))
  ; (10007 10009 10037)
  (out (search-for-primes 100000 search-up-bound 3))
  ; (100003 100019 100043) 
  )

; (runtime) is too small to come up with a conclusion 
; we need more test ...
