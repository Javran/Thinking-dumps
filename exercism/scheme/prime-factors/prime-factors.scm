(import (rnrs))

(use-modules (srfi srfi-41))


(define primes
  (stream-cons
   2
   (stream-cons
    3
    (stream-cons
     5
     (letrec ([gen (stream-lambda
                    (x)
                    (stream-cons
                     (+ x 1)
                     (stream-cons
                      (+ x 5)
                      (gen (+ x 6)))))])
       ;; generate possible primes base the fact that
       ;; every prime > 6 must be 6k+1 or 6k+5 for some k.
       ;; this is because:
       ;; - 6k+0, 6k+2, 6k+4 are obviously even
       ;; - 6k+3 is already divisible by 3.
       (let ([possible-primes (gen 6)])
         (define (prime? x)
           (stream-null?
            (stream-filter
             (lambda (y) (zero? (remainder x y)))
             (stream-take-while
              (lambda (y) (<= (* y y) x))
              primes))))
         (stream-filter prime? possible-primes)))))))

(define (prime? n)
  (and (> n 1)
       (or (= n 2)
           (stream-null?
            (stream-filter
             (lambda (p) (zero? (remainder n p)))
             (stream-take-while
              (lambda (p) (<= (* p p) n))
              primes))))))


(define (factorize n)
  (let loop ([rev-result '()]
             [cur-prime (stream-car primes)]
             [next-primes (stream-cdr primes)]
             [val n])
    (cond
     [(<= val 1) (reverse! rev-result)]
     [(prime? val) (reverse! (cons val rev-result))]
     [else
      (if (zero? (remainder val cur-prime))
          (loop (cons cur-prime rev-result)
                cur-prime
                next-primes
                (quotient val cur-prime))
          (loop rev-result
                (stream-car next-primes)
                (stream-cdr next-primes)
                val))])))
