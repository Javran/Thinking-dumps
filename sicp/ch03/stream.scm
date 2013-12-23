(define (stream-enumerate-interval low high)
  (if (> low high)
    nil
    (cons-stream low (stream-enumerate-interval
                       (+ low 1)
                       high))))

(define list-in-range-stream
  stream-enumerate-interval)

(define (display-stream s)
  (stream-for-each display-line s))

(define display-line
  ; I personally do not like
  ;   to have newline outputed
  ;   before any meaningful info outputed
  out)
