(load "../common/utils.scm")
(load "../common/test-utils.scm")
(load "./5_3_polynominal_setup.scm")
(load "./exercise_2_87_changes.scm")
(load "./exercise_2_88_changes.scm")
(load "./exercise_2_89_changes.scm")

; previously we have 
;   the normal termlist represented using tag `poly-termlist` 
;   and dense termlist in ex 2.89 represented using tag `poly-termlist-dense`
; so it's already done.
;   
; but here's another problem:
;   `add` and `mul` can only be applied to arguments of either
;   type `(poly-termlist poly-termlist)` or type `(poly-termlist-dense poly-termlist-dense)`
; coercion system might not work because the relationship
;   between poly-termlist and poly-termlist-dense is not suitable to be described
;   in terms of `project` and `raise`
; instead, I'll make two converters so these two types can be converted to each other.
(load "./exercise_2_90_changes.scm")

(run-test 'poly-termlist-dense-package)

(let ((y (make-termlist-from-args-dense
           1 (make-scheme-number 2)
           3 (make-scheme-number 4))))
  (out y)
  
  )

(end-script)
