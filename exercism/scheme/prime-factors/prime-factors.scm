(import (rnrs))

(use-modules (srfi srfi-41))

(define (factorize n)
  'implement-me!)

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
       ;; generate possible primes base the fact that every prime > 6 must be 6k+1 or 6k+5 for some k.
       ;; this is because:
       ;; - 6k+0, 6k+2, 6k+4 are obviously even
       ;; - 6k+3 is already divisible by 3.
       (let ([possible-primes (gen 6)])
         (define (is-prime x)
           (stream-null?
            (stream-filter
             (lambda (y) (zero? (remainder x y)))
             (stream-take-while (lambda (y) (<= (* y y) x)) primes))))
         (stream-filter is-prime possible-primes)))))))

(display (stream->list (stream-take 1500 primes)))
