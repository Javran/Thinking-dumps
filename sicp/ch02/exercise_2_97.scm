(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./exercise_2_97_setup.scm")

(let* ((t1 (make-tl-from-cseq-num
             'poly-termlist-sparse
             1 2 3))
       (t2 (make-tl-from-cseq-num
             'poly-termlist-sparse
             1 -1))
       (t3 (make-tl-from-cseq-num
             'poly-termlist-sparse
             2 -3 5 -7))
       (t4 (make-tl-from-cseq-num
             'poly-termlist-sparse
             8 -6 4 -2 0))
       (t5 (mul t1 (mul t2 t3)))
       (t6 (mul t1 (mul t2 t4))))
  (out (reduce t5 t6)))

(end-script)
