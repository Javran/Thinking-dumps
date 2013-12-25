(define (integrate-series s)
  (define coeffs
    (stream-map (lambda (x) (/ 1 x))
                integers))
  ((zip-streams-with *) coeffs s))

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
