;; a collection of testcases that might have side effects to the system

(define (qeval-tests)
  (qeval-initialize!)
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

  (qeval-initialize!)
  'ok)

(if *qeval-tests*
    (qeval-tests)
    'ok)

;; Local variables:
;; proc-entry: "./qeval.scm"
;; End:
