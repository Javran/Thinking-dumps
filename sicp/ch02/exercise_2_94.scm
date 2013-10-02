(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./exercise_2_94_setup.scm")

(let* ((p1 (make-poly
             'x
             (make-tl-from-cseq-num
               'poly-termlist-sparse
               1 2 3 4)))
       (p2 (make-poly
             'x
             (make-tl-from-cseq-num
               'poly-termlist-sparse
               2 2)))
       (d (div p1 p2)))
  (out (to-string (car d))
       (to-string (cadr d))))

(newline)
(let* ((p1 (make-polynomial
             'x '((4 1) (3 -1) (2 -2) (1 2))))
       (p2 (make-polynomial
             'x '((3 1) (1 -1)))))
  (out (to-string p1)
       (to-string p2)))

(newline)
(let* ((t1 (make-tl-from-cseq-num
             'poly-termlist-sparse
             1 2 1))
       (t2 (make-tl-from-cseq-num
             'poly-termlist-sparse
             1 3 3 1)))
  (out (my-gcd t1 t2)))

(newline)
(let* ((p1 (make-polynomial
             'x '((4 1) (3 -1) (2 -2) (1 2))))
       (p2 (make-polynomial
             'x '((3 1) (1 -1))))
       (res (my-gcd p1 p2)))
  (out "p1" (to-string p1))
  (out "p2" (to-string p2))
  (out "gcd(p1,p2)" (to-string res)))

;                         -x^2 +   0 +2
;            --------------------------
; -x^2+x^1+0 |  x^4 -x^3 -2x^2 +2x^1 +0
;               x^4 -x^3 
;               -----------------------
;                        -2x^2 +2x^1 +0
;                                     0
;                        --------------
;                        -2x^2 +2x^1 +0
;                        -2x^2 +2x^1 +0
;                        ==============



; (1)x^3+(-1)x^1


; gcd (-1)x^2+(1)x^1



(end-script)
