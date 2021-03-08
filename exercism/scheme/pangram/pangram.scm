(import (rnrs))

(use-modules ((srfi srfi-1) #:select (iota every)))
(use-modules ((srfi srfi-13)))

(define alphabet-ints
  (iota 26 (char->integer #\a)))

(define (pangram? phrase)
  (let ([xs (string-downcase phrase)])
    (and
     (every
      (lambda (y)
        (string-index xs (integer->char y)))
      alphabet-ints)
     #t)))

