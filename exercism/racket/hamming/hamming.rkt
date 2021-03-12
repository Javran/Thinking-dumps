#lang racket

(provide hamming-distance)

(define (hamming-distance xs ys)
  (let ([l (string-length xs)])
    (if (= l (string-length ys))
        (stream-count
         (lambda (i)
           (not (char=? (string-ref xs i) (string-ref ys i))))
         (in-range 0 l))
        (raise (error 'invalid)))))