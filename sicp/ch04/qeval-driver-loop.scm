(define (query-driver-loop)
  (define (display-stream s)
    (define (display-stream-intern s)
      ;; stream->list would work, but
      ;; not for cases that the stream is infinite.
      ;; we print out lines as many as possible,
      ;; even we know that it does not terminate.
      (if (stream-null? s)
          'ok
          (begin
            (out (stream-car s))
            (display-stream-intern (stream-cdr s)))))
    (if (stream-null? s)
        ;; it's better to display something
        ;; instead of keeping scilence
        ;; when no result is available
        (out "qeval: no result")
        (display-stream-intern s)))

  (display "qeval> ")
  (let ((q (query-syntax-process (read))))
    (cond ((assertion-to-be-added? q)
           (add-rule-or-assertion! (add-assertion-body q))
           (out "qeval: assertion added to database.")
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

(define qeval-repl query-driver-loop)

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
