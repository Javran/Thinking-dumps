(load "../common/utils.scm")

(load "./4_3_data_directed_put_get.scm")
(load "./exercise_2_77_complex.scm")
(install-complex-package)
(define make-from-real-imag
  (get 'make-from-real-imag 'complex))
(define make-from-mag-ang
  (get 'make-from-mag-ang 'complex))

; answer #1:
; simply by inserting codes from ex 2.77 into the complex package
;   might not work because we need the following code to connect procedures
;   with their corresponding symbols:
(define (real-part x) (apply-generic 'real-part x))
(define (imag-part x) (apply-generic 'imag-part x))
(define (magnitude x) (apply-generic 'magnitude x))
(define (angle x) (apply-generic 'angle x))

(define (show-info x)
  (display "data dump: ") (display x) (newline)
  (display "real-part: ") (display (real-part x)) (newline)
  (display "imag-part: ") (display (imag-part x)) (newline)
  (display "magnitude: ") (display (magnitude x)) (newline)
  (display "angle: ") (display (angle x)) (newline))

; test complex numbers
(newline)

(define x (make-from-real-imag 3 4))
(show-info x)
; answer #2:
; let's see what's happening when we try to evaluate `(magnitude x)`
; magnitude -> apply-generic
; from the implementation of `apply-generic`, we find `apply-generic` unpacks
; data from the object, including its tag and contents, then searches through the table
; according to arguments' type and eventually fetches the corresponding procedure.
; the first unpacking operation splits tag `complex` from its content, i.e. (rect . (3 . 4))
(newline)
(out x)
(out (type-tag x)) ; -> complex
(out (contents x)) ; -> (rect 3 . 4)
; before puting `magnitude` for type `(complex)` into table,
; there's no such a function that deals with type 'complex
; that's the first time `apply-generic` is used (in order to retrieve its content,
; i.e. the object representing a complex number)
; after that, its content need further examine to distinct rect impl from polar one,
; that's the second time `apply-generic` is involved.
; finally, the content is processed according to its current type tag,
; i.e. either 'rect or 'polar
; so:
; * apply-generic got involved for 2 times
;   * the first time, object's content is dispatched to `magnitude` of type `(complex)`
;   * the second time, current object's content is dispatched to either
;     `magnitude` from 'rect package or `magnitude` from 'polar package
;     according to the tag. for example, the x shown above will eventually processed
;     by `magnitude` from 'rect package

(end-script)
