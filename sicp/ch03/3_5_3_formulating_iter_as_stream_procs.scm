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
(newline)


; write my own version of pi-approximation
(define (stream-sum s)
  ; (stream-sum s) = s0, s0+s1, s0+s1+s2, ...
  ;   where s = s0, s1, s2, ...
  (define stream-sum-aux
    (cons-stream
      0
      (add-streams s stream-sum-aux)))
  (drop 1 stream-sum-aux))

(define my-pi-stream
  ; make an env that allows local definitions
  ((lambda ()
    ; 1, 3, 5, 7, ...
    (define odd-stream
      (cons-stream
        1
        (stream-map
          ((curry2 +) 2)
          odd-stream)))
    ; 1, -1, 1, -1, ...
    (define sign-stream
      (cons-stream
        1
        (stream-map
          -
          sign-stream)))
    ; 1, -1/3, 1/5, -1/7, ...
    (define frac-stream
      ((zip-streams-with
         (lambda (sign odd)
           (* sign (/ 1 odd))))
       sign-stream
       odd-stream))
    (scale-stream
      (stream-sum frac-stream)
      4))))

(display-stream
  (take 8 (stream-map exact->inexact my-pi-stream)))

(end-script)
