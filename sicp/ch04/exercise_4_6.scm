(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./exercise_4_5_common.scm")
(load "./exercise_4_6_common.scm")

(out 
  (eval-let
    '(let ((x 1)
           (y 2)
           (z 3))
       (display y)
       (newline)
       (+ x y z))
    (the-environment)))
; 2 (outputed by evaluation)
; 6 (outputed by `out`)

(end-script)
