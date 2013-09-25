(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./exercise_2_93_setup.scm")

; test
(out (to-string
       (make-poly
         'x
         (make-tl-from-cseq-num
           'poly-termlist-sparse
           4 3 2 1 0))))

; we start this exercise on the base of exercise 2.92
; My plan is:
; * make a new tag `rational-p` to stand for polynominal `rational`s
; * copy code from `rational` and apply some modification accordingly
; * test code by using the example given by the exercise

(end-script)
