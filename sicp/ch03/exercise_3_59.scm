(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./stream.scm")

(load "./exercise_3_59_common.scm")

(print-few
  10
  (integrate-series
    (scale-stream ones 17)))

(define (series-sum n s)
  ; take first n elements from a series
  ;   and calculate their sum. 
  (fold-left
    +
    0
    (stream->list (take n s))))

(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

(print-few 10 exp-series)

(out "exp 1:")
(out (exact->inexact (series-sum 10 exp-series)))
; as a comparison
(out (exp 1))

; see definition of `cosine-series` in "./exercise_3_59_common.scm"
; see definition of   `sine-series` in "./exercise_3_59_common.scm"

(out "cosine 1:")
(out (exact->inexact (series-sum 10 cosine-series)))
(out (cos 1))

(out "sine 1:")
(out (exact->inexact (series-sum 10 sine-series)))
(out (sin 1))

(end-script)
