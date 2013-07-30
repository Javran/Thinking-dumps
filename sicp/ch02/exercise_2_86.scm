(load "../common/utils.scm")

; changes to the system needed to accommodate this:
; * modify constructors to prevent building a complex number
;   from complex numbers
; * use `add` `sub` ... instead of `+` `-` to deal with mathmatical operations
; * insert `sine` and `cosine` to take the place of `sin` and `cos`
; * (optional) implement `print-num` to visualize a number

(end-script)
