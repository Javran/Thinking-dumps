(load "../common/utils.scm")
(load "../common/test-utils.scm")

(define (my-stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
    the-empty-stream
    (cons-stream
      (apply proc (map stream-car argstreams))
      (apply my-stream-map
             (cons
               proc
               (map stream-cdr argstreams))))))

(define (pretty-string-xyz x y z)
  (format #f "(~A,~A,~A)"
          x y z))

(let ((xs (list->stream '(1 2 3)))
      (ys (list->stream '(a b c)))
      (zs (list->stream '(4 5 6))))
  (out
    (stream->list
      (my-stream-map pretty-string-xyz xs ys zs))))

(end-script)
