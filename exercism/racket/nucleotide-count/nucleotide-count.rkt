#lang racket

(provide nucleotide-counts)

(define keys (string->list "ACGT"))

(define (nucleotide-counts dna)
  (define m (make-hash))
  (for-each
   (lambda (ch) (hash-set! m ch 0))
   keys)
  (for-each
   (lambda (ch)
     (cond
      [(hash-ref m ch #f) => (lambda (x) (hash-set! m ch (+ x 1)))]
      [else (raise (error 'invalid))]))
   (string->list dna))
  (map (lambda (ch) (cons ch (hash-ref m ch 0))) keys))