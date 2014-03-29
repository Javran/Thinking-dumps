(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./stream.scm")

(define (drop-until pred s)
  (if (pred (head s))
    s
    (drop-until pred (tail s))))

(define (stream-limit s tolerance)
  (define zipped-stream
    ; zip 2 streams:
    ;   s0, s1, s2, ...
    ;   s1, s2, s3, ...
    ((zip-streams-with cons) s (tail s)))
  (cdr ; take the element from the second stream
    (head ; take the first pair
      (drop-until ; drop values until we reach the tolerance
        (lambda (pair)
          (< (abs (- (car pair)
                     (cdr pair)))
             tolerance))
        zipped-stream))))

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

(define (my-sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

(define (test-sqrt x)
  (format #t
    "x       = ~A~%~
     my-sqrt = ~A~%~
     sqrt    = ~A~%"
    x (my-sqrt x 1e-6) (sqrt x)))

(test-sqrt 10000)
(test-sqrt 10.23456)

(end-script)
