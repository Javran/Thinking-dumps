(define (stream-enumerate-interval low high)
  (if (> low high)
    nil
    (cons-stream low (stream-enumerate-interval
                       (+ low 1)
                       high))))

(define list-in-range-stream
  stream-enumerate-interval)
