(load "./3_1_3_the_cost_of_intro_assignment.scm")

; continue with previous example
(out "Sameness and change")
(newline)

; we can say that D1 and D2 are the same:
; * same computation behavior
; * D1 could be subsituted for D2 in any computation
;   without changing the result

; but surely W1 and W2 are not the same:
; * calls to W1 and W2 has distinct effects
; * W1 could not be subsituted for W2 in any computation
;   without changing the result

; referentially transparent:
; * equals can be subsituted for equals
; * `set!` violates referentially transparent
