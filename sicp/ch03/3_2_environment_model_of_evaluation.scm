(load "../common/utils.scm")
(load "../common/test-utils.scm")

; environment:
;   a sequence of frames
; frame:
;   a table of bindings
;   has a pointer to its enclosing environment
; binding:
;   associates variable names with their corresponding values
; value of a variable:
;   the first frame in the environment that contains a binding for that variable
;   elsewise, that the variable is said to be unbound
;   (the so-called `first` frame is the one
;   when we start looking up binding from inside to outside)
; shadow:
;   we go through the frames from inside to outside, the first one takes effect
;   and others if any get omitted, and called `shadowed`

; to show some examples
(let ((x 3)
      (y 5))
  ; frame I
  (out x y)
  ; 3 5
  (let ((z 6)
        (x 7))
    ; frame II, the `x` in frame I gets shadowed
    (newline)
    (out x y z)
    ; 7 5 6 
    )
  (let ((m 1)
        (y 2))
    ; frame III
    (newline)
    (out x y m)
    ; 3 2 1
    ))

; the interpretation of an expression can differ given different environment:
(newline)
(let ((+ (lambda (a b)
           (out "this `+` does nothing"))))
  (+ 1 2))

(end-script)
