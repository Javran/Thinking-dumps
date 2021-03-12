#lang racket

(require racket/control)

(provide isogram?)

(define (isogram? s)
  (reset
   (foldr
    (lambda (i acc)
      (if (member i '(#\- #\space))
          acc
          (if (set-member? acc i)
              (shift _ #f)
              (set-add acc i))))
    (list->set '())
    (string->list (string-downcase s)))
   #t))
  