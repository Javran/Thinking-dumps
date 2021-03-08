(import (rnrs))

(define (leap-year? year)
  (define (z? n)
    (zero? (remainder year n)))
  (and (z? 4) (or (not (z? 100)) (z? 400))))


