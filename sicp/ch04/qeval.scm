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

(for-each
 add-rule-or-assertion!
 '((lisps mit-scheme)
   (lisps racket)
   (lisps elisp)
   (lisps clojure)
   (doge wow cool)
   (doge such scheme)))

(define (inflate-query q)
  (lambda (frame)
    (instantiate-exp
     q
     frame
     (lambda (v f)
       (contract-question-mark v)))))

(out (stream->list
      (stream-map
       (inflate-query '(lisps (? x)))
       (qeval '(lisps (? x)) (singleton-stream '())))))

(out (stream->list
      (stream-map
       (inflate-query '(doge (? x) (? y)))
       (qeval '(doge (? x) (? y)) (singleton-stream '())))))

;; Local variables:
;; proc-entry: ""
;; End:
