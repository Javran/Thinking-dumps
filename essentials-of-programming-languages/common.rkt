#lang racket

(provide out)
(provide filter)
(provide foldl)
(provide concat)
(provide concat-map)

; output all arguments, line-by-line
(define out
  (lambda args
    (for-each
      (lambda (a) (display a) (newline))
      args)))

(define (filter pred lst)
  (if (null? lst)
    '()
    (if (pred (car lst))
      (cons (car lst)
            (filter pred (cdr lst)))
      (filter pred (cdr lst)))))

; fold from left using `proc`
(define (foldl proc init lst)
  (if (null? lst)
    init
    (foldl
      proc 
      (proc init (car lst))
      (cdr lst))))

; concat lists
(define (concat xs)
  (apply append xs))

; map and then concat
(define (concat-map proc . args)
  (concat (apply map (cons proc args))))
