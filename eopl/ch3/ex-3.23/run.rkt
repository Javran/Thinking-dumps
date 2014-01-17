#lang eopl

; I have a diff definition of foldl..
; (require "../../common.rkt")
(require "../../test-utils.rkt")

(require "./top.rkt")
(require "./data-structures.rkt")

(require (only-in racket/file file->lines))
(require (only-in racket/base foldl))

; no modification, skip tests
; (test-all)

; result in the book is 12

(define (out . args)
  (for-each
    (lambda (x)
      (display x)
      (newline))
    args))

(out
  (expval->num
    (run "let makemult = proc (maker)
                           proc (x)
                             if zero?(x)
                               then 0
                               else -(((maker maker) -(x,1)), -4)
          in let times4 = proc (x) ((makemult makemult) x)
             in (times4 3)")))
(newline)

(define (run-source-file path)
  (run
    (foldl
      ; my impl ... two arguments are flipped
      (lambda (i acc)
        (string-append acc " " i))
      ""
      (file->lines path))))

(define (factorial x)
  (if (= x 0) 1 (* x (factorial (- x 1)))))

(out
  (expval->num
    (run-source-file "./factorial.proc"))
  (factorial 8))
