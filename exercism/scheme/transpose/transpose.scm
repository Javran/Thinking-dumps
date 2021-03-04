(import (rnrs))

(use-modules
 ((srfi srfi-1)
  #:select (every)))

;; TODO: I suspect there's a way simpler solution.

(define (transpose matrix)
  (let loop ([mat matrix]
             [result '()])
        (if (every null? mat)
            (reverse! result)
            (let ([col (map car mat)]
                  [remaining (map cdr (filter (lambda (x)
                                                (not (null? x))) mat))])
              (loop remaining (cons col result))))))
