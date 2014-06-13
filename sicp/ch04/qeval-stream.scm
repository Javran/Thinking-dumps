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
