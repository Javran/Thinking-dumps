(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./stream.scm")
(load "./exercise_3_59_common.scm")
(load "./exercise_3_60_common.scm")
(load "./exercise_3_61_common.scm")
(load "./exercise_3_62_common.scm")

(define tangent-series
  (div-series
      sine-series
    cosine-series))

(out
  ; tg 1 = sin 1 / cos 1
  (exact->inexact
    (series-sum
      100
      tangent-series)))
(out (tan 1))

(end-script)
