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

;; if two sets are equal
;; (order is not taken into account)
(define (set-equal? s1 s2)
  (cond ((null? s1) (null? s2))
        ((null? s2) (null? s1))
        (else
         (set-equal?
          (cdr s1)
          (delete (car s1) s2)))))

(do-test
 set-equal?
 (list
  (mat '(1 2 3) '(3 2 1) #t)
  (mat '(1 (a b (c (d))))
       '((a b (c (d))) 1) #t)
  (mat '() '(1 2) #f)
  (mat '(1 2) '() #f)))

(define (qeval-simple-query-tests)
  (qeval-initialize!)

  (for-each
   add-rule-or-assertion!
   '((lisps mit-scheme)
     (lisps racket)
     (lisps elisp)
     (lisps clojure)
     (doge wow cool)
     (doge such scheme)
     (list (a b c d) (c d e f))
     (list (a b c g) ())))

  (do-test
   (compose stream->list
            qeval4test)
   (list
    (mat '(lisps ?x)
         '((lisps mit-scheme)
           (lisps racket)
           (lisps elisp)
           (lisps clojure)))
    (mat '(dogs ?x)
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
    )
   set-equal?)

  (qeval-initialize!))

(define (qeval-tests)
  (qeval-simple-query-tests)
  'ok)

(if *qeval-tests*
    (qeval-tests)
    'ok)

;; Local variables:
;; proc-entry: "./qeval.scm"
;; End:
