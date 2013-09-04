; now let's get started with ex 2.92:
; Exercise 2.92: By imposing an ordering on variables, extend the
; polynomial package so that addition and multiplication of poly-
; nomials works for polynomials in different variables. (This is not
; easy!)
; * since variables are symbols, we can simply convert symbols into strings
;   so they will be compariable and easy to impose an ordering
; * I'd like to introduce a new procedure: `extract` which accepts a variable `var`
;   and a polynominal `poly1`, and produces a polynomial `poly2` which is mathematically
;   equal to `poly1` but the variable is guaranteed to be `var` and all of `poly2`'s coeffs
;   will not contain any piece of `var`.
;   e.g. extract `x` from `(x+1)*y^2+(x^2)*y+(x^2+x+1)` results in
;   `(y+1)*x^2+(y^2+1)*x+(y^2+1)` where x will not be found in any of poly's coeffs
; * with the help of `extract`, we'll be able to come up with a method of imposing
;   an ordering on variables, that is:
;     1. collect all variables in a poly
;     2. sort them
;     3. according to the sorted list of variables, apply `extract` in order
;   we can make sure the ordering is imposed by the guarantee that `extract` has made:
;     given a sorted variable list, say `var1, var2, var3 ...`
;     `extract var1 poly` removes all ocurrence of `var1` from `poly`'s coeffs
;     so the problem is now imposing ordering on `var2 var3 ...`, which is capable to be worked out
;     recursively
