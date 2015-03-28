;;; primitives

(define (user-print . args)
  (define (user-print-one obj)
    (cond ((compound-procedure? obj)
           (begin
             (pretty-print
              (list 'compound-procedure
                    (procedure-parameters obj)
                    (procedure-body obj)
                    '<procedure-env>))
             (newline)))
          ((compiled-procedure? obj)
           (out '<compiled-procedure>))
          (else (out obj))))
  (for-each user-print-one args))
