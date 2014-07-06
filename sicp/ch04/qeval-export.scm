(define (qe-stream-with-frames query frame-stream)
  (define frame-extern->intern
    query-syntax-process)
  (let ((q (query-syntax-process query)))
    (stream-map (inflate-query q)
                (qeval q
                       (stream-map frame-extern->intern
                                   frame-stream)))))

(define (qe-stream query)
  (qe-stream-with-frames
   query
   (singleton-stream empty-frame)))

(define qe-all
  (compose stream->list
           qe-stream))

(define (qe-asserts! . args)
  (for-each
   (compose
    add-rule-or-assertion!
    query-syntax-process)
   args))

(define (qe-fresh-asserts! . args)
  (qeval-initialize!)
  (apply qe-asserts! args))

;; Local variables:
;; proc-entry: "./qeval.scm"
;; End:
