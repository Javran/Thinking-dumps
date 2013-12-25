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

(define (x-seq x)
  ; (x^0, x^1, x^2, ...)
  ; => (x-seq x) = (1, x * (x-seq x))
  (define x-seq-aux 
    (cons-stream
      1
      (scale-stream x-seq-aux x)))
  x-seq-aux)

(print-few
  10
  (x-seq 2))

(end-script)
