(load "../common/utils.scm")

; define things
(define size 2)
(out
  2
  (* 5 size))

(define pi 3.14159)
(define radius 10)

(out (* pi radius radius))
; 314.15...

(define circumference (* 2 pi radius))
(out circumference)
; 62.83...

; * REPL keep track of name-object bindings, 
;     so we can use them in future
;     that is the (global) environment
