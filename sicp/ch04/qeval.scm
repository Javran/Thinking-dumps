(load "../common/utils.scm")
(load "../common/test-utils.scm")

(define *qeval-tests* #t)

(load "./qeval-base.scm")
(load "./qeval-transform.scm")
(load "./qeval-frames.scm")

(load "./qeval-get-put.scm")
(load "./qeval-stream.scm")
(load "./qeval-database.scm")

(load "./qeval-pattern.scm")
(load "./qeval-rules-and-unif.scm")

(load "./qeval-simple-query.scm")
(load "./qeval-compound-queries.scm")
(load "./qeval-filters.scm")

(load "./qeval-driver-loop.scm")

;; initialize everything
(define (qeval-initialize!)
  (proc-table-initialize!)
  (set! THE-ASSERTIONS the-empty-stream)
  (set! THE-RULES the-empty-stream))

(load "./qeval-tests.scm")


;; Local variables:
;; proc-entry: ""
;; End:
