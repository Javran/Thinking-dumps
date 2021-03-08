(define (accumulate f xs)
  (if (null? xs)
      '()
      (cons (f (car xs))
            (accumulate f (cdr xs)))))

