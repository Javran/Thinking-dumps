(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./stream.scm")

(define integers (integers-starting-from 1))

(for-each
  (lambda (ind)
    (out (stream-ref integers ind)))
  (list-in-range 0 8))
(newline)

(define (divisible? x y)
  (= (remainder x y) 0))

(define no-sevens
  (stream-filter
    (lambda (x)
      (not (divisible? x 7)))
    integers))

(out (stream-ref no-sevens 100))
(newline)

(define (fibgen a b)
  (cons-stream
    a
    (fibgen b (+ a b))))

(define fibs (fibgen 0 1))

(for-each
  (lambda (ind)
    (out (stream-ref fibs ind)))
  (list-in-range 0 8))
(newline)

; let me try to write `sieve` by myself.
(define (my-sieve stream)
  (let ((p (stream-car stream)))
    (cons-stream
      p
      (my-sieve
        (stream-filter
          (lambda (n)
            (not (divisible? n p)))
          (stream-cdr stream))))))

(define my-primes
  (my-sieve (integers-starting-from 2)))

(for-each
  (lambda (ind)
    (out (stream-ref my-primes ind)))
  (list-in-range 0 8))
(newline)

(out (stream-ref my-primes 50))
(newline)

(end-script)
