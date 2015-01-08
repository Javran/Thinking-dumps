(define error-symbol
  (gensym))

(define error?
  (list-tagged-with error-symbol))

(define (make-error . args)
  `(,error-symbol ,@args))

(define error-info cdr)

