(load "../common/utils.scm")

(define (compose f g)
  (lambda (x) (f (g x))))

(out ((compose square inc) 6))
