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

; my own version of pi-approximation
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
(newline)

; implementation from book
(define (pi-summands n)
  ; 1/n - 1/(n+2) + 1/(n+4) - ...
  (cons-stream (/ 1.0 n)
               (stream-map - (pi-summands (+ n 2)))))

(define pi-stream
  (scale-stream (parital-sums (pi-summands 1)) 4))

(display-stream
  (take 8 (stream-map exact->inexact pi-stream)))
(newline)

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))  ; s[n-1]
        (s1 (stream-ref s 1))  ; s[n  ]
        (s2 (stream-ref s 2))  ; s[n+1]
        )
    (cons-stream
      (- s2 (/ (square (- s2 s1))
               (+ s0 (* -2 s1) s2)))
      (euler-transform (stream-cdr s)))))

(display-stream
  (take 8 (stream-map
            exact->inexact
            (euler-transform
              my-pi-stream))))
(newline)

; even better ...
(display-stream
  (take 8 (stream-map
            exact->inexact
            (euler-transform
              (euler-transform
                my-pi-stream)))))
(newline)

; a stream of stream yielded by
;   consecutively applying `transform` on `s`.
(define (make-tableau transform s)
  (cons-stream
    s
    (make-tableau transform (transform s))))

(define (accelerated-sequence transform s)
  ; take the first element from each stream
  (stream-map stream-car (make-tableau transform s)))

(display-stream
  (take 8 (stream-map
            exact->inexact
            (accelerated-sequence euler-transform my-pi-stream))))
(newline)

(end-script)
