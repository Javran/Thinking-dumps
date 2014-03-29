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
(define B (make-accumulator 0))

(A 10)
; 15
(B 11)
; 11
(A 10)
; 25
(B 112)
; 123

(end-script)
