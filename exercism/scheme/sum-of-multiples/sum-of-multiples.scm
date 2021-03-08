(import (rnrs))

(use-modules ((srfi srfi-1) #:select (any)))

(define (sum-of-multiples ints-pre limit)
  (define ints (filter (lambda (x) (not (zero? x))) ints-pre))
  (let loop ([acc 0]
             [i 1])
    (if (>= i limit)
        acc
        (loop
         (if (any (lambda (x) (zero? (remainder i x))) ints)
             (+ acc i)
             acc)
         (+ i 1)))))
