(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./exercise_2_95_setup.scm")

(let* ((p1 (make-poly
             'x
             (make-tl-from-cseq-num
               'poly-termlist-sparse
               1 -2 1)))
       (p2 (make-poly
             'x
             (make-tl-from-cseq-num
               'poly-termlist-sparse
               11 7)))
       (p3 (make-poly
             'x
             (make-tl-from-cseq-num
               'poly-termlist-sparse
               13 5)))
       (q1 (mul p1 p2))
       (q2 (mul p1 p3))
       )
  (out "q1" (to-string q1))
  (out "q2" (to-string q2))
  (out "gcd(q1,q2)" (to-string (my-gcd q1 q2)))
  ; result:
  ; (36/13)x^2+(-72/13)x^1+(36/13)x^0 
  )


(end-script)
