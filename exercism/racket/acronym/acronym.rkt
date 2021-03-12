#lang racket

(provide acronym)

(define (acronym test)
  (let ([xs (string-split
             test
             #rx"[^a-zA-Z']+")])
    (list->string
     (filter-map (lambda (x)
                   (and (not (zero? (string-length x)))
                        (char-upcase (string-ref x 0))))
                 xs))))