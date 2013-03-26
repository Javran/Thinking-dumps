(load "../common/utils.scm")

(out
  (+ 1 
    (call/cc
      (lambda (k)
        (+ 2 (k 3))))))

; for (call/cc ...) part, the rest part is "(+ 1 [])"
;     here "[]" is used for place holder
; I think the rest part "(+ 1 [])" is "the current continuation"
;     and is bound to "k" in "(lambda (k) ...)"
; the computation "(+ 2 ...)" is aborted, and replaced with "(k 3)"
;     so the final result is "(+ 1 3)", i.e. 4

