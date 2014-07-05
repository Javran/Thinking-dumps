(define (display-stream s)
  (for-each out (stream->list s)))

(define (query-driver-loop)
  (display "qeval> ")
  (let ((q (query-syntax-process (read))))
    (cond ((assertion-to-be-added? q)
           (add-rule-or-assertion! (add-assertion-body q))
           (out "qeval: Assertion added to database.")
           (query-driver-loop))
          (else
           (display-stream
            (stream-map
             (lambda (frame)
               (instantiate-exp
                q
                frame
                (lambda (v f)
                  (contract-question-mark v))))
             (qeval q (singleton-stream '()))))
           (query-driver-loop)))))

;; evaluate assertions / rules / queries
;; on a stream of frames
(define (qeval query frame-stream)
  (let ((qproc (get (type query) 'qeval)))
    (if qproc
        (qproc (contents query) frame-stream)
        (simple-query query frame-stream))))

;; Local variables:
;; proc-entry: "./qeval.scm"
;; End:
