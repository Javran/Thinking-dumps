#lang eopl

(require "../../common.rkt")
(require "../../test-utils.rkt")

(require "./lang.rkt")
(require "./top.rkt")
(require "./data-structures.rkt")
(require "./interp.rkt")

(require rackunit)
(require (only-in racket sort))

(test-all)

; scan, parse a string into an expression
(define (string->exp s)
  (define pgm (scan&parse s))
  (cases program pgm
    (a-program (exp)
      exp)))

(define (string->free-variables s)
  (define (less? a b)
    (string<=? (symbol->string a)
               (symbol->string b)))
  (sort
    (free-variables (string->exp s))
    less?))

(check-equal? (string->free-variables "a") '(a))
(check-equal? (string->free-variables "10") '())
(check-equal? (string->free-variables "proc (x) -(x,10)") '())
(check-equal? (string->free-variables "proc (x) -(x,-(b,a))") '(a b))
