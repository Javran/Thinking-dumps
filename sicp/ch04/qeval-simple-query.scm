;; dependencies:
;; - qeval-stream
;; - qeval-pattern
;; - qeval-rule-and-uniq

(define (simple-query query-pattern frame-stream)
  (stream-intermap
   (lambda (frame)
     (stream-append-delayed
      ;; search against assertions
      (find-assertions query-pattern frame)
      ;; search against rules
      (delay (apply-rules query-pattern frame))))
   frame-stream))

;; Local variables:
;; proc-entry: "./qeval.scm"
;; End:
