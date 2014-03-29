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
  ; q1:
  ; (11)x^3+(-15)x^2+(-3)x^1+(7)x^0
  (out "q2" (to-string q2))
  ; q2:
  ; (13)x^3+(-21)x^2+(3)x^1+(5)x^0
  (out "gcd(q1,q2)" (to-string (my-gcd q1 q2)))
  ; result:
  ; (36/13)x^2+(-72/13)x^1+(36/13)x^0 
  )

; level #1:
;                                                                  (11/13)x^0
;                             -----------------------------------------------
; 13x^3 - 21x^2 + 3^x1 + 5x^0 | 11x^3 -       15x^2 -       3x^1 +       7x^0
;                               11x^3 - (231/13)x^2 + (33/13)x^1 + (55/13)x^0
;                               ---------------------------------------------
;                                        (36/13)x^2 - (72/13)x^1 + (36/13)x^0
; level #2:
;                                                       (169/36)x^1 + (65/36)x^0
;                                      -----------------------------------------
; (36/13)x^2 - (72/13)x^1 + (36/13)x^0 | 13x^2 - 21x^2 +       3x^1 +       5x^0
;                                        13x^2 - 26x^2 +      13x^1
;                                        ---------------------------------------
;                                                 5x^2 -      10x^1 +       5x^0
;                                                 5x^2 -      10x^1 +       5x^0
;                                                 ==============================
; result: (36/13)x^2 - (72/13)x^1 + (36/13)x^0

(end-script)
