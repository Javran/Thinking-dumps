(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./stream.scm")
(load "./exercise_3_59_common.scm")
(load "./exercise_3_60_common.scm")
(load "./exercise_3_61_common.scm")

(define (test-with-precision p)
  (define x (exact->inexact (series-sum p cosine-series)))
  (define y (exact->inexact (series-sum p (invert-unit-series cosine-series))))
  (format
    #t
    "step   : ~A~%~
     cos 1  : ~A~%~
     1/cos 1: ~A~%~
     product: ~A~%"
    p x y (* x y)))

(for-each
  test-with-precision
  '(10 25 50 100))

(end-script)
