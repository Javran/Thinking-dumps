#lang eopl

(require "../eopl3/chapter3/proc-lang/ds-rep/top.scm")
(require "../eopl3/chapter3/proc-lang/ds-rep/data-structures.scm")

(require (only-in racket/file file->lines))
(require (only-in racket/base foldl format printf))

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

(define proc-code-template
  (load-source-file "./ex-3.24-mutual.proc"))

(define (int->bool x)
  (not (= 0 x)))

(define (interp-even x)
  (int->bool
    (expval->num
      (run
        (format
          "~A (even ~A)"
          proc-code-template
          x)))))
      
(define (interp-odd x)
  (int->bool
    (expval->num
      (run
        (format
          "~A (odd ~A)"
          proc-code-template
          x)))))

(for-each
  (lambda (x)
    (printf
      "~A: even? ~A odd? ~A~%"
      x (interp-even x) (interp-odd x)))
  '(1 2 3 4 5))
