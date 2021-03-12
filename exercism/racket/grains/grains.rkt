#lang racket

(provide square total)

(define (square n)
  (if (<= 1 n 64)
      (arithmetic-shift 1 (- n 1))
      (raise (error'invalid))))

(define total
  (let ([answer (- (arithmetic-shift 1 64) 1)])
    (lambda () answer)))