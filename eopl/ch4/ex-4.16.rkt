#lang eopl

(require "../common.rkt")
(require "../test-utils.rkt")

(require "../eopl3/chapter4/implicit-refs/top.scm")
(require "../eopl3/chapter4/implicit-refs/data-structures.scm")
(require "../eopl3/chapter4/implicit-refs/store.scm")
(require "../eopl3/chapter4/implicit-refs/interp.scm")

(instrument-newref #t)
(instrument-let #t)

(out (expval->num (run "
let times4 = 0
in begin
     set times4 =
       proc (x)
         if zero?(x)
           then 0
           else -((times4 -(x,1)), -4);
     (times4 3)
   end
")))
