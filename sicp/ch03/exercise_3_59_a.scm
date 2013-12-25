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

(end-script)
