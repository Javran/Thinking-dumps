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
  (define to-poly ((curry2 make-poly) 'x))
  (define out-poly (compose out to-string))
  (let* ((p1 (to-poly t5))
         (p2 (to-poly t6))
         (reduce-result (reduce p1 p2))
         (pp1 (car reduce-result))
         (pp2 (cadr reduce-result)))
    (out "p1")
    (out-poly p1)
    (out "p2")
    (out-poly p2)
    (out "reduced p1")
    (out-poly pp1)
    (out "reduced p2")
    (out-poly pp2)))

(newline)
(define (skip)
(let ((a (make-rational-g 10 20))
      (b (make-rational-g
           (make-poly
             'x
             (make-tl-from-cseq-num 
               'poly-termlist-sparse
               1 2 1))
           (make-poly
             'x
             (make-tl-from-cseq-num
               'poly-termlist-sparse
               1 1)))))
  ; generic make-rational works
  ;   for both integers and polynomials
  (out (to-string a))
  (out (to-string b)))
)

; these two are equal!
; -(x^4 + x^3 + x^2 - 2*x - 1)/(- x^5 + x^3 + x^2 - 1)
; (x^3 + 2*x^2 + 3*x + 1)/(x^4 + x^3 - x - 1)
; TODO: show that there're equal
(let ()
  (define  p1 (make-polynomial 'x '((1 1) (0  1))))
  (define  p2 (make-polynomial 'x '((3 1) (0 -1))))
  (define  p3 (make-polynomial 'x '((1 1))))
  (define  p4 (make-polynomial 'x '((2 1) (0 -1))))
  (define rf1 (make-rational-g p1 p2))
  (define rf2 (make-rational-g p3 p4))
  (define result (add rf1 rf2))
  (out (to-string rf1))
  (out (to-string rf2))
  (out (to-string result))
  )

(end-script)
