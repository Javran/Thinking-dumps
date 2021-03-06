(import (rnrs))

(use-modules ((srfi srfi-41)))

(define pascal-triangles
  (stream-iterate (lambda (xs) (map + (cons 0 xs) (append xs '(0)))) '(1)))

(define (pascals-triangle n)
  (stream->list (stream-take n pascal-triangles)))
