(define (query-driver-loop)
  (define (display-stream s)
    (define display-element-limit 100)
    (define (display-stream-intern s limit)
      ;; stream->list would work, but
      ;; not for cases that the stream is infinite.
      (if (stream-null? s)
          'ok
          (if (< limit display-element-limit)
              (begin
                (out (stream-car s))
                (display-stream-intern
                 (stream-cdr s)
                 (add1 limit)))
              (out "qeval: output limit reached"))))
    (if (stream-null? s)
        ;; it's better to display something
        ;; instead of keeping silence
        ;; when no result is available
        (out "qeval: no result")
        (display-stream-intern s 0)))

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
