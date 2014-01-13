#lang eopl

(require "../common.rkt")
(require "../test-utils.rkt")

(define (identifier? s)
  (and (not (eq? 'lambda s))
       symbol?)) 

(define-datatype lc-exp lc-exp?
  (var-exp
    (var identifier?))
  (lambda-exp
    (bound-var identifier?)
    (body lc-exp?))
  (app-exp
    (rator lc-exp?)
    (rand lc-exp?)))

; will raise error when uncommented
; (define exp-test1 (var-exp 'lambda))

(define exp-test2 (var-exp 'x))
