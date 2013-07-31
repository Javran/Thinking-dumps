(load "../common/utils.scm")

; changes to the system needed to accommodate this:
; * modify constructors to prevent building a complex number
;   from complex numbers
; * use `add` `sub` ... instead of `+` `-` to deal with mathmatical operations
; * insert `sine` and `cosine` to take the place of `sin` and `cos`
; * (optional) implement `print-num` to visualize a number

; package requirement:
; * constructors
; * accessors:
;   denom/numer for rational
;   real-part/imag-part/magnitude/angle for complex numbers
; * mathematical operations:
;   add/sub/mul/div/equ?/=zero?
; * coercion system:
;   raise/project
; * (optional) visualization
;   print-num

; test tools
(load "./exercise_2_86_test.scm")

; tag system
(load "./exercise_2_86_tag_system.scm")

(end-script)
