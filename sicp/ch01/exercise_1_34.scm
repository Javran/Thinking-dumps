(load "../common/utils.scm")

; the meaning of "f" is to apply 2 to the target function
(define (f g) (g 2))

(out (f square))
; 4

(out (f (lambda (z) (* z (+ z 1)))))
; 6

; if we apply "f" to "f"
; then "f" will apply 2 to its parameter, i.e.: "f"
; (f f) => (f 2) => (2 2)
; and (2 2) cannot be executed since the first element is not a procedure

(out (f f))
; the error will be "2 is not applicatable"
