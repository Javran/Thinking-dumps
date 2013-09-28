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
             'x ((3 1) (1 -1)))))
  (out (to-string p1)
       (to-string p2)))

; plan:
; * seems we no longer needs variable ordering system
;     remove this feature in future packages as it is over-complicated
;     and not easy to implement something like =zero? correctly.

(end-script)
