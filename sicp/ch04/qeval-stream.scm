;; extra stream operations for qeval
;; status: working, tested
(load "../common/utils.scm")
(load "../common/test-utils.scm")

;; same as "stream-append" except that
;; the second argument is delayed
(define (stream-append-delayed s1 delayed-s2)
  (if (stream-null? s1)
      (force delayed-s2)
      (cons-stream
       (stream-car s1)
       (stream-append-delayed
        (stream-cdr s1)
        delayed-s2))))

;; same as "interleave" except that
;; the second argument is delayed
(define (interleave-delayed s1 delayed-s2)
  (if (stream-null? s1)
      (force delayed-s2)
      (cons-stream
       (stream-car s1)
       (interleave-delayed
        (force delayed-s2)
        (delay (stream-cdr s1))))))

;; using "flatmap" here would lead to confusion
;; instead I use "stream-intermap" to indicate
;; that the flatten process is done by taking
;; the interleave of streams
(define (stream-intermap proc s)
  (define (interleave-flatten-stream stream)
    (if (stream-null? stream)
        the-empty-stream
        (interleave-delayed
         (stream-car stream)
         (delay
           (interleave-flatten-stream
            (stream-cdr stream))))))
  (interleave-flatten-stream (stream-map proc s)))

(define (singleton-stream x)
  (cons-stream x the-empty-stream))

(define (qeval-stream-tests)
  ;; convert between stream and list for
  ;; some functions
  (define (func-converter func)
    (lambda (s1 s2)
      (stream->list
       (func
        (list->stream s1)
        (delay (list->stream s2))))))
  ;; stream-append-delayed tests
  (let ((testcases
         (list
          (mat '(1 2 3) '(4 5 6) '(1 2 3 4 5 6))
          (mat '() '() '())
          (mat '(1 2 3) '() '(1 2 3))
          (mat '() '(1 2 3) '(1 2 3)))))
    (do-test (func-converter stream-append-delayed) testcases))
  ;; interleave-delayed tests
  (let ((testcases
         (list
          (mat '(1 2) '(3 4 5 6) '(1 3 2 4 5 6))
          (mat '() '(1 2 3) '(1 2 3))
          (mat '(1 2 3 4) '(5 6) '(1 5 2 6 3 4)))))
    (do-test (func-converter interleave-delayed) testcases))
  ;; stream-intermap tests
  (let ((testcases
         (list
          (mat (lambda (x)
                 (list->stream
                  (list x x)))
               '(1 2 3)
               '(1 2 1 3 2 3))))
        (func (lambda (proc s)
                (stream->list
                 (stream-intermap
                  proc
                  (list->stream s))))))
    (do-test func testcases))
  ;; singleton-stream tests
  (let ((testcases
         (list
          (mat 1 '(1))
          (mat 'a '(a))))
         (func
          (lambda (x)
            (stream->list
             (singleton-stream x)))))
    (do-test func testcases))
  'ok)

(qeval-stream-tests)
