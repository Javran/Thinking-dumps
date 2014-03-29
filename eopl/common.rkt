#lang eopl

(require (only-in racket/base
                  format))

(provide out)
(provide filter)
(provide my-foldl)
(provide concat)
(provide concat-map)
(provide assert)
(provide curry2)
(provide memf)
(provide const)
(provide identity)
(provide format)
(provide non-empty?)
(provide compose)
(provide flip)

; compose unaries
; compose (...  h . g . f) x = ... $ h $ g $ f x
; => compose-inv (f . g . h ...) x
; => (compose-inv (g . h ...)) $ f x
(define (compose . procs)
  (define (compose-inv procs)
    (if (null? procs)
      identity
      (lambda (x)
        ((compose-inv (cdr procs)) ((car procs) x)))))
  (let ((procs-inv (reverse procs)))
    (compose-inv procs-inv)))

; flip f x y = f y x
(define (flip f)
  (lambda (a b) (f b a)))

; to prove readability of `pair?` that actually means
;   a non-empty stuff
(define non-empty? pair?)

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
(define (my-foldl proc init lst)
  (if (null? lst)
    init
    (my-foldl
      proc 
      (proc init (car lst))
      (cdr lst))))

; concat lists
(define (concat xs)
  (apply append xs))

; map and then concat
(define (concat-map proc . args)
  (concat (apply map (cons proc args))))

; assertion with optional reason
(define (assert v . reason)
  (if v
    'done
    (eopl:error
      'assert
      (string-append
        "Assertion failed"
        (if (null? reason)
          ""
          (string-append
            ": " (car reason)))))))

; return curried version of a binary function
(define (curry2 f)
  (lambda (a)
    (lambda (b)
      (f a b))))

; memf: (a -> Bool) -> [a] -> Either a Bool
; search through `xs`, and get the list starting from the first element that
;   satisfies `pred`, return #f if `xs` does not have an element
;   that meets the predicate.
(define (memf pred xs)
  (if (null? xs)
    #f
    (if (pred (car xs))
      xs
      (memf pred (cdr xs)))))

; return `v` whatever is given
(define (const v)
  (lambda ignored
    v))

; identity function
(define (identity x)
  x)
