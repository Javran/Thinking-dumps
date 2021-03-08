(import (rnrs))

(define (hamming-distance xs ys)
  (if (= (string-length xs) (string-length ys))
      (apply + (map (lambda (x y) (if (eq? x y) 0 1))
                    (string->list xs)
                    (string->list ys)))
      (raise 'invalid)))

