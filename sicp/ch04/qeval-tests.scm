;; a collection of testcases that might have side effects to the system

(define (qeval-tests)
  (qeval-initialize!)

  (qeval-initialize!)
  'ok)

(if *qeval-tests*
    (qeval-tests)
    'ok)

;; Local variables:
;; proc-entry: "./qeval.scm"
;; End:
