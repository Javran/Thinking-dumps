#lang eopl

(require "../common.rkt")
(require "../test-utils.rkt")

(require (prefix-in cbn: "../eopl3/chapter4/call-by-need/top.scm"))
(require (prefix-in cbn: "../eopl3/chapter4/call-by-need/data-structures.scm"))

(require (prefix-in cbv: "../eopl3/chapter3/proc-lang/ds-rep/top.scm"))
(require (prefix-in cbv: "../eopl3/chapter3/proc-lang/ds-rep/data-structures.scm"))

(define prog-str "
  let makerec =
    proc (f)
      let d =
        proc (x)
         proc (z)
           ((f (x x)) z)
      in proc (n)
           ((f (d d)) n)
  in let maketime4 =
       proc (f)
         proc (x)
           if zero?(x)
             then 0
             else -((f -(x,1)),-4)
  in let times4 = (makerec maketime4)
  in (times4 3)
  ")

(define prog-str-eta "
  let makerec =
    proc (f)
      let d = proc (x) (f (x x))
      in (f (d d))
  in let maketime4 =
       proc (f)
         proc (x)
           if zero?(x)
             then 0
             else -((f -(x,1)),-4)
  in let times4 = (makerec maketime4)
  in (times4 3)
  ")

; they should both work
(out (cbv:expval->num (cbv:run prog-str)))
(out (cbn:expval->num (cbn:run prog-str)))
(newline)

; not working for call-by-value
;(out (cbv:expval->num (cbv:run prog-str-eta)))
(out (cbn:expval->num (cbn:run prog-str-eta)))

; `prog-str-eta` is not working using call-by-value strategy
;   because the evaluation is eager.
; for eager evaluation, the line `(f (d d))` will
;   end up in infinite loop because the operand `(d d)` requires
;   to be evaluated which leads to non-termination.
