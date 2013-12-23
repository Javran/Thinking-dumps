(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./stream.scm")

(define ones (cons-stream 1 ones))

(define (zip-streams-with proc)
  (lambda args
    (apply stream-map (cons proc args))))

(define add-streams (zip-streams-with +))

(define integers
  (cons-stream 1 (add-streams ones integers)))

(print-few 10 integers)

(define fibs
  (cons-stream
    0
    (cons-stream
      1
      (add-streams (drop 1 fibs) fibs))))

(print-few 10 fibs)

(define (scale-stream stream factor)
  (stream-map
    ((curry2 *) factor)
    stream))

(define double (cons-stream 1 (scale-stream double 2)))

(print-few 10 double)

(define (divisible? x y)
  (= (remainder x y) 0))
(define (prime? n)
  (define (iter ps)
    (cond ((> (square (stream-car ps)) n)
           #t)
          ((divisible? n (stream-car ps))
           #f)
          (else (iter (stream-cdr ps)))))
  (iter primes))
(define primes
  ; I guess this one is not so efficient as the previous one
  (cons-stream
    2
    (stream-filter prime? (integers-starting-from 3))))

(print-few 10 primes)

(end-script)
