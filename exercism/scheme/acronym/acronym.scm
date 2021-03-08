(import (rnrs))

(use-modules ((srfi srfi-1) #:select (filter-map)))

(define (acronym test)
  (let ([xs (string-split
             test
             (lambda (x)
               (not (or (char-alphabetic? x)
                        (eq? x #\')))))])
    (list->string
     (filter-map (lambda (x)
                   (and (not (zero? (string-length x)))
                        (char-upcase (string-ref x 0))))
                 xs))))

