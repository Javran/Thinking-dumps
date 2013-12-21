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
