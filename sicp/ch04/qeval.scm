(load "../common/utils.scm")
(load "../common/test-utils.scm")

(define *qeval-tests* #f)

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
(load "./qeval-export.scm")
(load "./qeval-tests.scm")

(qe-fresh-asserts!)

;; Local variables:
;; proc-entry: ""
;; End:
