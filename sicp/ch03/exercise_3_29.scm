(load "../common/utils.scm")
(load "../common/test-utils.scm")

; truth table:
; a b  '!a and !b' 'a or b'
; 1 1  0            1
; 1 0  0            1
; 0 1  0            1
; 0 0  1            0

; (logical-or a b) = (logical-not
;                      (logical-and
;                        (logical-not a)
;                        (logical-not b)))


#|
           ------
a -> o -> |      \
          |  and  |-> o ->
b -> o -> |      /
           ------
|#



(end-script)
