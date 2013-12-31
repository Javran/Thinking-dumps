(load "../common/utils.scm")
(load "../common/test-utils.scm")

; the structure of `(pairs s t)`

; <s_0,t_0> | <s_0,t_1>   <s_0,t_2> ...
; ----------+-----------+--------------
;           | <s_1,t_1> | <s_1,t_2> ...
;           +-----------+--------------
;           |           | <s_2,t_2> ...

; first piece | second piece
; ------------+-------------
;             | third  piece

; first  piece: the heads
; second piece: (head s) + (tail t)
; third  piece: recursive structure

(end-script)
