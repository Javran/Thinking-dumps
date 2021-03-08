(import (rnrs))

(define keys (string->list "ACGT"))

(define (nucleotide-count dna)
  (define m (make-eq-hashtable 4))
  (for-each
   (lambda (ch) (hashtable-set! m ch 0))
   keys)
  (for-each
   (lambda (ch)
     (cond
      [(hashtable-ref m ch #f) => (lambda (x) (hashtable-set! m ch (+ x 1)))]
      [else (raise 'invalid)]))
   (string->list dna))
  (map (lambda (ch) (cons ch (hashtable-ref m ch 0))) keys))

