(import (rnrs))

(define (binary-search vec target)
  (let loop ([l 0]
             [r (- (vector-length vec) 1)])
    (if (> l r)
        'not-found
        (let* ([mid (quotient (+ l r) 2)]
               [x (vector-ref vec mid)])
          (cond
           [(= x target) mid]
           [(< x target) (loop (+ mid 1) r)]
           [else (loop l (- mid 1))])))))
