#lang eopl

(require "../../common.rkt")
(require "../../test-utils.rkt")

(require "./top.rkt")
(require "./data-structures.rkt")

(require (only-in racket/file file->lines))

; no modification, skip tests
; (test-all)

; result in the book is 12

(out
  (expval->num
    (run "let makemult = proc (maker)
                           proc (x)
                             if zero?(x)
                               then 0
                               else -(((maker maker) -(x,1)), -4)
          in let times4 = proc (x) ((makemult makemult) x)
             in (times4 3)")))

(define (run-source-file path)
  (run
    (foldl
      ; my impl ... two arguments are flipped
      (lambda (acc i)
        (string-append acc " " i))
      ""
      (file->lines path))))

(out
  (expval->num
    (run-source-file "./factorial.proc")))

(define (factorial x)
  (if (= x 0) 1 (* x (factorial (- x 1)))))

(out (factorial 8))
