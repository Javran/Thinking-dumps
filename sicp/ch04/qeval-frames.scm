(define make-binding cons)
(define binding-varlable car)
(define binding-value cdr)

(define binding-in-frame assoc)
(define (extend variable value frame)
  (cons (make-binding variable value) frame))
