(load "../common/utils.scm")

; summary:
; '(vector ... )' or '#( ... )' to make a vector
; 'make-vector' to init a vector with specified size
; 'vector-ref' to access elements
; 'vector-set!' to modify elements
; 'vector?' to test if something is a vector

(out (vector 1 #\a #t))
; #(1 #\a #t)

; 2d-vector:
(define vector-2d #(
       #( 0 1 2 )
       #( 3 4 5 )
       #( 6 7 8 )))

(out vector-2d)

; access last line
(out (vector-ref vector-2d 2))
; #(6 7 8)

; modify the mid element
(vector-set! (vector-ref vector-2d 1) 1 "mid")
(out vector-2d)

(out (vector? "aaa"))
; #f
(out (vector? #()))
; #t
(out (vector? #(1 2 3)))
; #t
