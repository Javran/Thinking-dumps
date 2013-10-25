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

(end-script)
