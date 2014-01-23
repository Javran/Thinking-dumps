#lang eopl

(require "../common.rkt")
(require "../test-utils.rkt")

(require "../eopl3/chapter4/explicit-refs/data-structures.scm")
(require "../eopl3/chapter4/explicit-refs/top.scm")

(out (expval->num (run
  "let g = proc (dummy)
             let counter = newref(0)
             in begin
                  setref(counter, -(deref(counter),-1));
                  deref(counter)
                end
   in let a = (g 11)
     in let b = (g 11)
       in -(a,b)
       ")))
