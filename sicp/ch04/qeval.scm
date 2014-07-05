(load "../common/utils.scm")
(load "../common/test-utils.scm")

(define *qeval-tests* #t)

(load "./qeval-utils.scm")

(load "./qeval-stream.scm")
(load "./qeval-data-directed.scm")
(load "./qeval-frame.scm")
(load "./qeval-syntax-trans.scm")
(load "./qeval-database.scm")
(load "./qeval-pattern-matching.scm")
(load "./qeval-unification.scm")
(load "./qeval-handlers.scm")
(load "./qeval-driver-loop.scm")

;; initialize everything
(define (qeval-initialize!)
  (proc-table-initialize!)
  (install-handlers)
  (set! THE-ASSERTIONS the-empty-stream)
  (set! THE-RULES the-empty-stream))

(load "./qeval-tests.scm")

;; Local variables:
;; proc-entry: ""
;; End:
