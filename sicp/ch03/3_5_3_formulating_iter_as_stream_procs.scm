(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./stream.scm")

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (cons-stream
      1.0
      (stream-map
        (lambda (guess)
          (sqrt-improve guess x))
        guesses)))
  guesses)

(display-stream (take 5 (sqrt-stream 2)))

(end-script)
