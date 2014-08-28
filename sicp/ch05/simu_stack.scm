(define (empty-stack) (vector '()))

(define (stack-push! st e)
  (vector-modify! st 0 (lambda (stack)
                         (cons e stack))))
(define (stack-pop! st)
  (vector-modify! st 0 cdr))
(define (stack-top st)
  (car (vector-ref st 0)))
