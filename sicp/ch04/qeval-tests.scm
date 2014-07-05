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

(define (qeval-query-tests)
  (qeval-initialize!)

  ;; cover database queries and
  ;; pattern matching and unification
  ;; in real database queries
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
     (only-a a)
     (num 1)
     (num 2)
     (num 3)
     (rule (connect ?a ?b)
           (or (edge ?a ?b)
               (and (edge ?a ?c)
                    (connect ?c ?b))))
     (rule (test-or1 ?a)
           (or (doge wow ?a)
               (edge a ?a)))
     (rule (test-or2 ?a)
           (or (only-a ?a)
               (edge ?a b)))
     (rule (test-or3 ?a)
           (or (no such)
               (assertions)))
     (rule (test-and1 ?a)
           (and (doge wow ?a)
                (edge a ?a)))
     (rule (test-and2 ?a)
           (and (only-a ?a)
                (edge ?a b)))
     (rule (test-and3 ?a)
           (and (no such)
                (assertions)))
     (rule (test-not1 ?a)
           (not (edge ?a ?b)))
     ))

  (do-test
   (compose stream->list
            qeval4test)
   (list
    ;; simple query test
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
    ;; compound query test
    (mat '(connect a ?x)
         '((connect a b)
           (connect a c)
           (connect a d)))
    ;; "or" handler
    (mat '(or (doge wow ?a)
              (edge a ?a))
         '((or (doge wow cool) (edge a cool))
           (or (doge wow b) (edge a b))))
    (mat '(or (only-a ?a) (edge ?a b))
         '((or (only-a a) (edge a b))))
    (mat '(or (no such) (an assertion))
         '())
    ;; "and" handler
    (mat '(and (doge wow ?a)
               (edge a ?a))
         '())
    (mat '(and (only-a ?a) (edge ?a b))
         '((and (only-a a) (edge a b))))
    (mat '(and (no such) (an assertion))
         '())
    ;; "not" handler
    (mat '(not (edge b c))
         '())
    (mat '(not (edge a d))
         '((not (edge a d))))
    ;; "lisp-value" handler
    (mat '(and (num ?a) (num ?b)
               (lisp-value < ?a ?b))
         '((and (num 1) (num 2)
                (lisp-value < 1 2))
           (and (num 2) (num 3)
                (lisp-value < 2 3))
           (and (num 1) (num 3)
                (lisp-value < 1 3))))
    ;; "always-true" handler
    (mat '(always-true)
         '((always-true)))
    (mat '(and (always-true) (num ?a))
         '((and (always-true) (num 1))
           (and (always-true) (num 2))
           (and (always-true) (num 3))))
    )
   set-equal?)

  (qeval-initialize!))

(define (qeval-tests)
  (qeval-database-tests-db)
  (qeval-query-tests)
  'ok)

(if *qeval-tests*
    (qeval-tests)
    'ok)

;; Local variables:
;; proc-entry: "./qeval.scm"
;; End:
