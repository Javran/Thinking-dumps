#lang racket

(require srfi/13)

(provide to-rna)

(define (to-rna dna)
  (string-map
   (lambda (ch)
     (case ch
       [(#\C) #\G]
       [(#\G) #\C]
       [(#\T) #\A]
       [(#\A) #\U]))
   dna))