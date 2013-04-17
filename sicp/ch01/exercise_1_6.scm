(load "../common/utils.scm")

; the difference between 'if' and 'new-if' is:
; if-expression is special that only evaluate one of the branch 
;     according to the predication
; but "new-if" will get all its branch evaluated
;     since the "else-clause" always get evaluated,
;     Alyssa would finally suffer from an infinite loop: sqrt-iter -> sqrt-iter -> ...
