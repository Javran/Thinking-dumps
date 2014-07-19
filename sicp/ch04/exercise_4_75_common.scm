(define (singleton-stream? s)
  (and (not (stream-null? s))
       (stream-null? (stream-cdr s))))

(define (uniquely-asserted query-pattern frame-stream)
  (stream-intermap
   (lambda (frame)
     (let ((result
            (qeval (car query-pattern)
                   (singleton-stream frame))))
       (if (singleton-stream? result)
           result
           the-empty-stream)))
   frame-stream))

(define (install-handler-unique)
  (put 'unique 'qeval uniquely-asserted))
