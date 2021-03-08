(import (rnrs))

(define (square n)
  (if (<= 1 n 64)
      (ash 1 (- n 1))
      (raise 'invalid)))

(define total
  (- (ash 1 64) 1))
