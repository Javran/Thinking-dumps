(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./stream.scm")

(load "./exercise_3_74_common.scm")
(load "./exercise_3_75_common.scm")

; What I hate about this `make-zero-crossings` is that:
; * confuses "last raw value" with "last output value"
;   (this is also where bug comes from)
; * try to make 2 steps in a single procedure, i.e.
;   calculate the average + construct the stream

(define (make-zero-crossings-wrong
          input-stream
          last-value)
  (cons-stream
    (sign-change-detector
      (stream-car input-stream)
      last-value)
    (make-zero-crossings-wrong
      (stream-cdr input-stream)
      (stream-car input-stream))))

(define (make-zero-crossings-2
          input-stream
          last-raw-value
          last-out-value)
  (let ((avpt (average (stream-car input-stream)
                       last-raw-value)))
    (cons-stream
      (sign-change-detector avpt last-out-value)
      (make-zero-crossings-2
        (stream-cdr input-stream)
        (stream-car input-stream)
        avpt))))

(define zero-crossings
  (make-zero-crossings-2
    test-stream
    0
    0))

(define zero-crossings-wrong
  (make-zero-crossings-wrong test-stream 0))

(out "correct:")
(print-few 10 zero-crossings)
(out "wrong:")
(print-few 10 zero-crossings-wrong)

; but why not convert a stream to its average stream first
(define (sense-average s)
  (stream-map
    average
    s
    (cons-stream 0 s)))

; now we can reuse the code from ex 3.74
(out "reuse ex 3.74:")
(define zero-crossings-o
  (make-zero-crossings
    (sense-average test-stream)
    0))

(print-few 10 zero-crossings-o)

(end-script)
