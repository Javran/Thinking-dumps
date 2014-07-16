(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./qeval.scm")

(define (simple-stream-flatmap proc s)
  ;; analogy to concat-map
  (simple-flatten (stream-map proc s)))

(define (simple-flatten stream)
  (stream-map
   ;; after the filter is applied,
   ;; we only have singletons
   ;; safe to convert them back into elements
   stream-car
   (stream-filter
    ;; the stream is either an empty one or a singleton
    (lambda (s) (not (stream-null? s)))
    stream)))

(define (2d-list->stream xs)
  (list->stream
   (map list->stream xs)))

(do-test
 (lambda (x)
   (let ((s (2d-list->stream x)))
     (map stream->list
          (list (stream-intermap identity s)
                (simple-stream-flatmap identity s)))))
 (list
  (mat '((1) () () (2) (3) (4)) 'ignored)
  (mat '(() () () ()) 'ignored)
  (mat '((a) (b) (c) (d)) 'ignored))
 (lambda (actual expected)
   (apply equal? actual)))

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
