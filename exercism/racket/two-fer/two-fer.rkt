#lang racket

(provide two-fer)

(define (two-fer . maybe-name)
  (if (null? maybe-name)
      "One for you, one for me."
      (string-append
       "One for " (car maybe-name) ", one for me.")))