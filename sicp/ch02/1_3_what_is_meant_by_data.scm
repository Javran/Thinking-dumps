(load "../common/utils.scm")

; not every set of procedures can serve as an appropriate implementation
; they require some conditions enforced by the situation under which the data is used
; for example:
; (= (/ (numer x) (denom x)) (/ n d)) should always be true 
;     and is irrelevant to how make-rat is implemented

; we can make our version of cons/car/cdr in a different way:

(define (cons-2 x y)
  (define (dispatch m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (else (error "Argument not 0 or 1: cons-2: " m))))
  dispatch)

(define (car-2 z)
  (z 0))

(define (cdr-2 z)
  (z 1))

(define data (cons-2 1 (cons-2 2 3)))

(out (car-2 data)           ; 1
     (car-2 (cdr-2 data))   ; 2
     (cdr-2 (cdr-2 data)))  ; 3

; * message-passing *
; This example also demonstrates that 
;   the ability to manipulate procedures as objects 
;   automatically provides the ability to represent compound data.
