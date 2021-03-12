#lang racket

(provide my-length
         my-reverse
         my-map
         my-filter
         my-fold
         my-append
         my-concatenate)

;; I'm just implementing everything with foldr for fun

(define (my-foldr proc init xs)
  (match xs
    [(cons y ys)
     (proc y (my-foldr proc init ys))]
    ['() init]))

(define my-length
  (curry my-foldr (lambda (_ acc) (add1 acc)) 0))

(define (my-reverse xs)
  ((my-foldr
    (lambda (i acc) (compose1 acc (curry cons i)))
    identity
    xs)
   '()))
  
(define (my-map f xs)
  (my-foldr (lambda (i acc) (cons (f i) acc)) '() xs))

(define (my-filter pred xs)
  ((my-foldr
    (lambda (i acc)
      (compose1 (if (pred i) (curry cons i) identity)
                acc))
    identity
    xs)
   '()))
  
(define (my-fold proc init xs)
  ((my-foldr (lambda (i acc)
               (lambda (acc2)
                 (acc (proc i acc2))))
             identity xs)
   init))

(define (my-append xs ys)
  (my-foldr cons ys xs))
  
(define my-concatenate
  (curry my-foldr my-append '()))