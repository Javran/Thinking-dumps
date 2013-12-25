(define (integrate-series s)
  (define coeffs
    (stream-map (lambda (x) (/ 1 x))
                integers))
  ((zip-streams-with *) coeffs s))

(define (series-sum n s)
  ; take first n elements from a series
  ;   and calculate their sum. 
  (fold-left
    +
    0
    (stream->list (take n s))))

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
