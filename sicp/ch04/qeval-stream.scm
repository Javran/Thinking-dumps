;; extra stream operations for qeval

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
      (force s2)
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
