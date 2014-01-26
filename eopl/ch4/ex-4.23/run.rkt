#lang eopl

(require "../../common.rkt")
(require "../../test-utils.rkt")

(require "./top.rkt")
(require "./data-structures.rkt")
(require "./printer.rkt")

(test-all)

; based on ex 4.22

(print-to-screen #t)

; read an integer `n`, print from 1 to `n`
(out "please input an integer:")
(run "
  var n,i;
  { read n;
    i = 1;
    while not(zero?(-(+(n,1),i)))
    {
      print i;
      i = +(i,1)
    }

   }
   ")
