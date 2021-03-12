#lang racket

(provide sum-of-squares square-of-sum difference)

(define (square-of-sum n)
  (let ([m (quotient (* n (+ n 1)) 2)])
    (* m m)))

(define (sum-of-squares n)
  (quotient (* n (+ n 1) (+ n n 1)) 6))

(define (difference n)
  (quotient (* n (- n 1) (+ n 1) (+ n n n 2)) 12))