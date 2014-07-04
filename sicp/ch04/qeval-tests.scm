;; a collection of testcases that might have side effects to the system

(define (inflate-query q)
  (lambda (frame)
    (instantiate-exp
     q
     frame
     (lambda (v f)
       (contract-question-mark v)))))

;; qeval wrapped for tests
(define (qeval4test query)
  (let ((q (query-syntax-process query)))
    (stream-map
     (inflate-query q)
     (qeval q (singleton-stream '())))))

(define (qeval-database-tests-db)
  (qeval-initialize!)

  (for-each
   add-rule-or-assertion!
   '((assert1 foo)
     (assert1 bar)
     (assert2 a)
     (assert2 1 2 3 4)
     (rule (both (? x) (? y))
           (and (good (? x)) (good (? y))))
     (rule (good (? a)))
     (rule ((? not) (? indexable)))))

  (do-test
   fetch-assertions
   (list
    ;; indexable assertions
    (mat '(assert1 (? x)) 'not-used
         '((assert1 foo) (assert1 bar)))
    (mat '(assert2 (? y) 2 3 (? z)) 'not-used
         '((assert2 a) (assert2 1 2 3 4)))
    ;; not indexable, return all assertions
    (mat '((pat pat) (foo bar)) 'not-used
         '((assert1 foo) (assert1 bar) (assert2 a) (assert2 1 2 3 4))))
   (lambda (actual expected)
     (set-equal? (stream->list actual) expected)))

  (do-test
   fetch-rules
   (list
    ;; indexable rules
    (mat '(both 'a 'b) 'not-used
         '((rule (both (? x) (? y))
                 (and (good (? x)) (good (? y))))
           (rule ((? not) (? indexable)))))
    (mat '(good 't) 'not-used
         '((rule (good (? a)))
           (rule ((? not) (? indexable)))))
    ;; not indexable, return all rules
    (mat '((not indexable)) 'not-used
         '((rule  (both (? x) (? y))
                 (and (good (? x)) (good (? y))))
           (rule (good (? a)))
           (rule ((? not) (? indexable))))))
   (lambda (actual expected)
     (set-equal? (stream->list actual) expected)))

  (qeval-initialize!)
  'ok)

(define (qeval-simple-query-tests)
  (qeval-initialize!)

  (for-each
   (compose
    add-rule-or-assertion!
    query-syntax-process)
   '((lisps mit-scheme)
     (lisps racket)
     (lisps elisp)
     (lisps clojure)
     (doge wow cool)
     (doge such scheme)
     (list (a b c d) (c d e f))
     (list (a b c g) ())
     (edge a b)
     (edge b c)
     (edge c d)
     (rule (connect ?a ?b)
           (or (edge ?a ?b)
               (and (edge ?a ?c)
                    (connect ?c ?b))))
     ))

  (do-test
   (compose stream->list
            qeval4test)
   (list
    (mat '(lisps ?x)
         '((lisps mit-scheme)
           (lisps racket)
           (lisps elisp)
           (lisps clojure)))
    (mat '(doge ?x)
         '())
    (mat '(doge ?x ?y)
         '((doge wow cool)
           (doge such scheme)))
    (mat '(doge ?x scheme)
         '((doge such scheme)))
    (mat '(doge wow ?y)
         '((doge wow cool)))
    (mat '(list (a b c . ?x) ?y)
         '((list (a b c d) (c d e f))
           (list (a b c g) ())))
    (mat '(connect a ?x)
         '((connect a b)
           (connect a c)
           (connect a d)))
    )
   set-equal?)

  (qeval-initialize!))

(define (qeval-tests)
  (qeval-database-tests-db)
  (qeval-simple-query-tests)
  'ok)

(if *qeval-tests*
    (qeval-tests)
    'ok)

;; Local variables:
;; proc-entry: "./qeval.scm"
;; End:
