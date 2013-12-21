# Inefficiency

The first program:

    (define (sum-primes a b)
      (define (iter count accum)
        (cond (; count until we have reached `b`
               (> count b)
                 accum)
              ((prime? count)
                 (iter (+ count 1) (+ count accum)))
              (else
                 (iter (+ count 1) accum))))
      (iter a 0))

The second program:

    (define (sum-primes a b)
      (accumulate
        +
        0
        (filter
          prime?
          (enumerate-interval a b))))

In the second program, `filter` can do nothing before
`enumerate-interval` returns a result,
while in the first program, it keeps generating new element and
testing the generated one alternatively.

In terms of storage, the first program need to keep only two variables.
But the second program generates a intermediate list which is not necessary.

Consider a more extreme case:

    (car
      (cdr (filter
             prime?
             (enumerate-interval 10000 1000000))))

Only the head of the result is needed but in this program
we have to get all computation done even if it is not necessary.

# Stream

* partially construct a stream
* automatically construct just enough more of itself on demand
* the interface is just like list, with different names

constraints:

    (stream-car (cons-stream x y)) = x
    (stream-cdr (cons-stream x y)) = y

primitives:

* `cons-stream`
* `stream-car`
* `stream-cdr`
* `the-empty-stream`
* `stream-null?`

stream operations:

    (define (stream-ref s n)
      (if (= n 0)
        (stream-car s)
        (stream-ref (stream-cdr s) (- n 1))))

    (define (stream-map proc s)
      (if (stream-null? s)
        the-empty-stream
        (cons-stream
           (proc (stream-car s))
           (stream-map proc (stream-cdr s)))))

    (define (stream-for-each proc s)
      (if (stream-null? s)
        'done
        (begin
          (proc (stream-car s))
          (stream-for-each proc (stream-cdr s)))))

    (define (display-stream s)
      (stream-for-each display-line s))
    (define (display-line x)
      (newline) (display x))

# Procedure `delay` and `force`

* `delay`: create "delayed object", a promise to evaluate
at some future time.
* `force`: perform evaluation for delayed objects.

To construct a stream, based on the representation of a list,
we leave the `cdr` part of a list as a "delayed object".

    (define (cons-stream a b)
      ; this implementation does not work
      ;   cons-stream has to be a special form
      ;   because if this is not the case,
      ;   `b` will be evaluated
      ;   before we construct the stream
      (cons a (delay b)))

    (define (stream-car s)
      (car s))

    (define (stream-cdr s)
      (force (cdr s)))
