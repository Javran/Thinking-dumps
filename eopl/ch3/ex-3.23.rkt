#lang eopl

(require "../eopl3/chapter3/proc-lang/ds-rep/top.scm")
(require "../eopl3/chapter3/proc-lang/ds-rep/data-structures.scm")

(require (only-in racket/file file->lines))
(require (only-in racket/base foldl))

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
      (lambda (i acc)
        (string-append acc " " i))
      ""
      (file->lines path))))

(define (factorial x)
  (if (= x 0) 1 (* x (factorial (- x 1)))))

(out
  (expval->num
    (run-source-file "./ex-3.23-factorial.proc"))
  (factorial 8))
