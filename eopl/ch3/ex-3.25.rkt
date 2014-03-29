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
    (run
"
let makerec =
  proc (f)
    let d =
      proc (x)
        proc (z)
          ((f (x x)) z)
    in proc (n) ((f (d d)) n)
in let maketimes4 =
  proc (f)
    proc (x)
      if zero?(x)
        then 0
        else -((f -(x,1)),-4)
   in let times4 = (makerec maketimes4)
      in (times4 3)
")))

; my own implementation
(out
  (expval->num
    (run
"
let pseudoY =
  proc (psy)
    proc (f)
      proc (x)
        ((f ((psy psy) f)) x)
in
let y =
  proc (x)
    ((pseudoY pseudoY) x)
in
let maketimes4 =
  proc (f)
    proc (x)
      if zero?(x)
        then 0
        else -((f -(x,1)),-4)
in let times4 = (y maketimes4)
in (times4 3)
")))
