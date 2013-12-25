(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./stream.scm")

(define (integrate-series s)
  (define coeffs
    (stream-map (lambda (x) (/ 1 x))
                integers))
  ((zip-streams-with *) coeffs s))

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

(define cosine-series
  (cons-stream
    1
    (scale-stream 
      (integrate-series sine-series)
      -1)))

(define sine-series
  (cons-stream
    0
    (integrate-series (scale-stream cosine-series 1))))

(out "cosine 1:")
(out (exact->inexact (series-sum 10 cosine-series)))
(out (cos 1))

(out "sine 1:")
(out (exact->inexact (series-sum 10 sine-series)))
(out (sin 1))

(end-script)
