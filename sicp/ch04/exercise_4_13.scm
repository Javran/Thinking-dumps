(load "../common/utils.scm")
(load "../common/test-utils.scm")

; * If I remove binding from the first frame where
;     the variable is found, we cannot guaranteed that 
;     this variable is now not being occupied by anything else.
;     If the intension is to make any queries about that variable
;     cause errors, this method might not work.
; * If I remove all the occurences of a variable
;     along the path of enclosing-environment
;     this will lead to dangerous situation.
;     where we might end up removing some important
;     primitive bindings and there's no way to bring
;     these primitives back.
; * The best thing I can do is to add an extra field
;     into the environment to keep a list of variables
;     that has been `masked`. If the variable is listed
;     in the `marked` section, we just simply report
;     that the binding does not exist.

(end-script)
