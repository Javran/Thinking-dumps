#lang eopl

(require "../eopl3/chapter3/proc-lang/ds-rep/top.scm")
(require "../eopl3/chapter3/proc-lang/ds-rep/data-structures.scm")

(require (only-in racket/file file->lines))
(require (only-in racket/base foldl))

(define (out . args)
  (for-each
    (lambda (x)
      (display x)
      (newline))
    args))

(define (load-source-file path)
  (foldl
    (lambda (i acc)
      (string-append acc " " i))
    ""
    (file->lines path)))

(out
  (expval->num
    (run
      (load-source-file "./ex-3.24-mutual.proc"))))
