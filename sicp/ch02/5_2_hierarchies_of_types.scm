(load "../common/utils.scm")

; a tower of types
;   complex
;   ^
;   |
;   real
;   ^
;   |
;   rational
;   ^
;   |
;   integer

; operation `raise`:
; we can raise an integer to a rational/real/complex number

; operation `lower`:
; we can lower a data object to its simplest representation
; for example, 6+0i can be simplified to an integer 6.

; when the system is required to operate on objects of different types,
; it can successively raise the lower types until all the objects
; are at the same level in the tower.

(end-script)
