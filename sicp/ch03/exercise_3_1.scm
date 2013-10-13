(load "../common/utils.scm")
(load "../common/test-utils.scm")

(define (make-accumulator init)
  (let ((count init))
    (define (accumulate diff)
      (set! count (+ count diff))
      (out count)
      count)
    accumulate))

(define A (make-accumulator 5))

(A 10)
(A 10)

(end-script)
