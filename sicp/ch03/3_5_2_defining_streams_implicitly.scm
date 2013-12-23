(load "../common/utils.scm")
(load "../common/test-utils.scm")

(define (take n stream)
  (if (or (= n 0) (stream-null? stream))
    the-empty-stream
    (cons-stream
      (stream-car stream)
      (take (- n 1) (stream-cdr stream)))))

(define (drop n stream)
  (if (or (= n 0) (stream-null? stream))
    stream
    (drop (- n 1) (stream-cdr stream))))


(define ones (cons-stream 1 ones))

(end-script)
