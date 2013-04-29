(load "../common/utils.scm")

(define (next x)
  (if (= x 2)
    3
    (+ x 2)))

(out "test 'next' with init seed 2:")
(out (take-iterate next 2 5))
; should be (2 3 5 7 9)
