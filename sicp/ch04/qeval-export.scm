;; perform query, from external representation
;; to external representation
(define (qe-stream-with-frames query frame-stream)
  (define frame-extern->intern
    query-syntax-process)
  ;; transform to internal representation
  (let ((q (query-syntax-process query)))
    (stream-map
     ;; transform back to external representation
     (inflate-query q)
     (qeval q
            (stream-map frame-extern->intern
                        frame-stream)))))

;; perform query using default empty frame
(define (qe-stream query)
  (qe-stream-with-frames
   query
   (singleton-stream empty-frame)))

;; perform query and convert the result into
;; a list
(define qe-all
  (compose stream->list
           qe-stream))

;; insert assertions and rules
(define (qe-asserts! . args)
  (for-each
   (compose
    add-rule-or-assertion!
    query-syntax-process)
   args))

;; wipe the database, and then
;; insert assertions and rules
(define (qe-fresh-asserts! . args)
  (qeval-initialize!)
  (apply qe-asserts! args))

;; Local variables:
;; proc-entry: "./qeval.scm"
;; End:
