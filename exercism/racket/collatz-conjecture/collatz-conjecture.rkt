#lang racket

(provide collatz)

(define/contract (collatz n)
  (positive-integer? . -> . integer?)
  (let loop ([x n]
             [i 0])
    (if (= x 1)
        i
        (loop
         (let-values ([(q r) (quotient/remainder x 2)])
           (if (zero? r)
               q
               (+ x x x 1)))
         (+ i 1)))))