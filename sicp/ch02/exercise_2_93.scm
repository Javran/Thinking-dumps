(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./exercise_2_93_setup.scm")

; test
(let* ((p1 (make-poly
             'x
             (make-tl-from-cseq-num
               'poly-termlist-sparse
               4 3 2 1 0)))
       (p2 (make-poly
             'x
             (make-tl-from-cseq-num
               'poly-termlist-sparse
               3 0 2 0 1)))
       (r (make-rational-p p1 p2)))
  (out (to-string r))
  (out (to-string (numer r)))
  (out (to-string (denom r))))

(newline)
(let* ((p1 (make-poly
             'x
             (make-tl-from-cseq-num
               'poly-termlist-sparse
               1 1 1)))
       (p2 (make-poly
             'x
             (make-tl-from-cseq-num
               'poly-termlist-sparse
               1 -1 -1)))
       (p3 (make-poly
             'x
             (make-tl-from-cseq-num
               'poly-termlist-sparse
               1 1)))
       (r1 (make-rational-p p1 p2))
       (r2 (make-rational-p p2 p1))
       (r3 (make-rational-p p3 p1)))
  (out (to-string r1))
  (out (to-string r2))
  (out (to-string r3))
  (out (to-string (sub r2 r3)))
  
  )

; we start this exercise on the base of exercise 2.92
; My plan is:
; * make a new tag `rational-p` to stand for polynominal `rational`s
; * copy code from `rational` and apply some modification accordingly
; * test code by using the example given by the exercise
; update:
; * maybe we should expose something like adjoin-term / mul-term-by-all-terms
;     and I guess this might make our life easier
; * impl `make-polynomial` to support the exercise

(end-script)
