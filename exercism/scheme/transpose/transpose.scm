(import (rnrs))

(define (transpose matrix)
  (if (null? matrix)
      '()
      (apply map (cons list matrix))))
