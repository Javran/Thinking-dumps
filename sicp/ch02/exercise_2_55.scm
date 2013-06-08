(load "../common/utils.scm")

(out (car ''aaa))
; quote
; 'x => (quote x)

(out (car '(quote aaa)))
; quote

(newline)
; I have 2 explanations, but I think only one of them is correct
; #1
(out (car (quote (quote aaa))))
; quote

; #2
(out (car (list 'quote 'aaa)))
; quote

(out (cdr ''aaa))
; (aaa)

(out (cddr ''aaa))
; ()

; still cannot tell
; I don't even know if anyone would define things like this...
;   or it would make any sense...

(end-script)
