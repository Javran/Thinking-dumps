(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./stream.scm")

; group consecutive equal numbers together,
;   and count occurrence.
; e.g. (1 1 2 2 3 4 5 5) => ((1 2) (2 2) (3 1) (4 1) (5 2))
(define (group-stream s)
  ; remove consecutive elements from head,
  ;   return a stream of lists (element occurrence)
  (if (stream-null? s)
    the-empty-stream
    (let loop ((cur-s s)
               (value (head s))
               (count 0))
      (cond ((stream-null? cur-s)
              (cons-stream
                (list value count)
                the-empty-stream))
            ((= (head cur-s) value)
              (loop (tail cur-s) value (+ count 1)))
            (else
              (cons-stream
                (list value count)
                (group-stream cur-s)))))))

(print-few 20 (group-stream (list->stream '(1 1 2 2 3 4 5 5 6))))

(end-script)
