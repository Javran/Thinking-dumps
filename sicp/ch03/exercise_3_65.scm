(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./stream.scm")

(define ln2
  ((lambda ()
    ; using lambda as environment
    ; 1, -1, 1, -1, ...
    (define sign-stream
      (cons-stream
        ; use inexact numbers to speed up
        1.
        (stream-map
          -
          sign-stream)))
    ; 1/1 - 1/2 + 1/3 - 1/4 + ...
    (define frac-stream
      ((zip-streams-with
         (lambda (sign int)
           (* sign (/ 1 int))))
       sign-stream
       integers))
    (stream-sum frac-stream))))

(out "target:" (log 2))
(newline)

(display-stream (take 10 (drop 1000 (stream-map exact->inexact ln2))))
(newline)
; this sequence converges relatively slow. 

; >>>> code from 3.5.3
(define (euler-transform s)
  (let ((s0 (stream-ref s 0))  ; s[n-1]
        (s1 (stream-ref s 1))  ; s[n  ]
        (s2 (stream-ref s 2))  ; s[n+1]
        )
    (cons-stream
      (- s2 (/ (square (- s2 s1))
               (+ s0 (* -2 s1) s2)))
      (euler-transform (stream-cdr s)))))
(define (make-tableau transform s)
  (cons-stream
    s
    (make-tableau transform (transform s))))

(define (accelerated-sequence transform s)
  ; take the first element from each stream
  (stream-map stream-car (make-tableau transform s)))
; <<<< code from 3.5.3

(define ln2-acc (accelerated-sequence euler-transform ln2))
(display-stream (take 10 (stream-map exact->inexact ln2-acc)))

(end-script)
