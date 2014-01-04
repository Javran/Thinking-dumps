(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./stream.scm")

(define (sign-change-detector
          new-val
          old-val)
  (define (sgn x)
    ; the sign of zero is positive
    (if (>= x 0) 1 -1))
  (let ((new-sgn (sgn new-val))
        (old-sgn (sgn old-val)))
    (if (= new-sgn old-sgn)
      0
      ; else there's a zero-crossing
      (if (= old-sgn 1)
        ; positive -> negative
        -1
        ; negative -> positive
        +1))))

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

(define (make-zero-crossings
          input-stream
          last-raw-value
          last-out-value)
  (let ((avpt (average (stream-car input-stream)
                       last-raw-value)))
    (cons-stream
      (sign-change-detector avpt last-out-value)
      (make-zero-crossings
        (stream-cdr input-stream)
        (stream-car input-stream)
        avpt))))

; test stream:
;   0, +2, 0, -2, 0, +2, 0, -2, ...
; expected averaged stream: 
; [0], 1, 1, -1, -1, 1, 1, -1, ...
; expected zero-crossings:
; [0], 0, 0, -1,  0, 1, 0, -1, ...
(define test-stream
  (cons-stream
    0
    (cons-stream
      2
      (cons-stream
        0
        (cons-stream
          -2 test-stream)))))

(define zero-crossings
  (make-zero-crossings
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
(define (make-zero-crossings-original
          input-stream
          last-value)
  (cons-stream
    (sign-change-detector
      (stream-car input-stream)
      last-value)
    (make-zero-crossings-original
      (stream-cdr input-stream)
      (stream-car input-stream))))

(out "reuse ex 3.74:")
(define zero-crossings-o
  (make-zero-crossings-original
    (sense-average test-stream)
    0))

(print-few 10 zero-crossings-o)

(end-script)
