(import (rnrs))

(define (keep pred seq)
  (if (null? seq)
      '()
      (let ([x (car seq)]
            [sub (keep pred (cdr seq))])
        (if (pred x) (cons x sub) sub))))

(define (discard pred seq)
  (keep (lambda (x) (not (pred x))) seq))
