(import (rnrs))

(define (collatz n)
  (let loop ([x n]
             [i 0])
    (if (= x 1)
        i
        (loop
         (if (zero? (remainder x 2))
             (quotient x 2)
             (+ x x x 1))
         (+ i 1)))))
