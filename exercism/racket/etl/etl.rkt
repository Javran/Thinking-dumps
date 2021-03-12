#lang racket

(provide etl)

(define/contract (etl input)
  ((hash/c positive-integer? (listof string?))
   . -> .
   (hash/c string? positive-integer?))
  (let ([h (make-hash)])
    (hash-for-each
     input
     (lambda (k vs)
       (for-each
        (lambda (v)
          (hash-set! h (string-downcase v) k))
        vs)))
    h))
     