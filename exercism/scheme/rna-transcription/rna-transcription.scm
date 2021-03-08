(import (rnrs))

(use-modules ((srfi srfi-13)))

(define (dna->rna dna)
  (string-map
   (lambda (ch)
     (case ch
       [(#\C) #\G]
       [(#\G) #\C]
       [(#\T) #\A]
       [(#\A) #\U]))
   dna))
