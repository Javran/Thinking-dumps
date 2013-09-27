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
  (out "poly-rat" (to-string r))
  (out "poly-rat-numer" (to-string (numer r)))
  (out "poly-rat-demon" (to-string (denom r))))

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
  (out "r1" (to-string r1))
  (out "r2" (to-string r2))
  (out "r3" (to-string r3))
  (out "add r2 r3" (to-string (add r2 r3)))
  (out "sub r2 r3" (to-string (sub r2 r3)))
  (out "mul r1 r2" (to-string (mul r1 r2)))
  (out "div r3 r2" (to-string (div r3 r2)))
  )

(let* ((p1 (make-polynomial 'x '((2 1) (0 1))))
       (p2 (make-polynomial 'x '((3 1) (0 1))))
       (rf (make-rational-p p2 p1))
       (rf2 (add rf rf)))
  (out "p1" (to-string p1))
  (out "p2" (to-string p2))
  (out "rf" (to-string rf))
  (out "rf2" (to-string rf2))
  )

; TODO (future work)
; * maybe we should expose something like adjoin-term / mul-term-by-all-terms

(end-script)
