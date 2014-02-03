#lang eopl

(require "../../common.rkt")
(require "../../test-utils.rkt")

; (require "./top.rkt")
(require "./data-structures.rkt")

; (test-all)

(define pair (make-continuation 'tetete boolean? integer?))

(define constructor (car pair))
(define extractor (cdr pair))

(define x (constructor #t 1234))

(extractor x
  (lambda (b i)
    (out (format "b=~A i=~A~%" b i))))
