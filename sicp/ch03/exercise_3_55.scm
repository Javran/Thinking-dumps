(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./stream.scm")

(define (partial-sums s)
  ; accumulate from 0, rather than the first element
  (define partial-sums-aux
    (cons-stream
      0
      (add-streams partial-sums-aux s)))
  ; drop the first element
  (stream-cdr partial-sums-aux))

(print-few 10 (partial-sums integers))

(end-script)
