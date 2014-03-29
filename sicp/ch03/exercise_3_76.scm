(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./stream.scm")

(load "./exercise_3_74_common.scm")
(load "./exercise_3_75_common.scm")

; exactly what I've done additionally in ex 3.74.

(define (sense-average s)
  (stream-map
    average
    s
    (cons-stream 0 s)))

(define smooth sense-average)

(define zero-crossings
  (make-zero-crossings
    (smooth test-stream)
    0))

(print-few 10 zero-crossings)

(end-script)
