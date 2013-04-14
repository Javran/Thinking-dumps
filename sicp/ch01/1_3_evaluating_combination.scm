(load "../common/utils.scm")

(define x (+ 3 5 7))

(out
  (* (+ 2 (* 4 6)) x))

; to evaluate a combination:
; 1. evaluate subexpressions(note 'recursive')
; 2. apply leftmost subexpr to values of other subexpr

; how to eval:
; * numerals -> number they name(itself)
; * built-in ops -> machine instruction seqs
; * _ -> search name in env

; note "eval rule" does not handle definition, (there are special forms)
;     i.e. (define x 1) does not mean to apply 'define' to '(x 1)
;         p.s. we know nothing about 'x' as well :)
