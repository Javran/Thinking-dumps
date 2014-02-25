#lang eopl

(require "../../common.rkt")
(require "../../test-utils.rkt")

(require "./top.rkt")
(require "./data-structures.rkt")

; based on ex 5.12
; based on ex 5.13

(test-all)
(newline)

(instrument-cont #t)

(out "starting proc: fact")
(eopl:printf "final answer: ~A~%"
  (expval->num
    (run
      "letrec fact (n) =
         if zero?(n)
           then 1
           else *(n,(fact -(n,1)))
       in (fact 4)
       ")))
(newline)

(out "starting proc: fact-iter")
(eopl:printf "final answer: ~A~%"
  (expval->num
    (run
      "letrec fact-iter (n) =
         letrec fact-iter-acc (n) =
           proc (a)
             if zero?(n)
               then a
               else ((fact-iter-acc -(n,1)) *(n,a))
         in ((fact-iter-acc n) 1)
       in (fact-iter 4)
       ")))

; (fact 4) => (* 4 (* 3 (* 2 (fact 1))))
; the cont => (* 4 (* 3 (* 2 <?>)))
