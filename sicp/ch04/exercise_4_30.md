# Question a

I think Ben is not totally right,
because the procedure passed to `for-each`
contains a primitive `display`, which is strict in
its argument `x`. The correctness of `for-each` thus
depends on the strictness of the procedure passed to it.
The evaluator handles this case correctly simply because
the procedure is strict in its argument.
if we change the whole procedure to:

```scheme
(define count 0)
(define (id! x)
  (set! count (+ count 1))
  x)

(for-each
  (lambda (x)
    (newline)
    (display 'foo))
  (map id!
       (list 57 321 88)))
```

The `for-each` will no longer work as expect,
because the procedure passed to `for-each`
is not strict in its argument.

# Question b

(I\'m not sure about this part)


With the original `eval-sequence`, the output will be:

```
(1 2)
1
```

The first line from the output is because `(p1 x)` is strict in `x`.
(`set!` triggers the application of `cons`,
which is a primitive.)

The second line is because that the evaluation of `e`
in the body of `p` does not force the value of `e`.
(`e` itself is an assignment, which does not force the
`(cons x '(2))` part inside of it.)

With Cy\'s proposed version, the result will be:

```
(1 2)
(1 2)
```

This is because each element in the sequence is forced.

# Question c

Cy\'s proposed version fully evaluates a sequence,
in this case, the value to be forced is just numbers
without side effects.
So Cy\'s version makes no difference in terms of program outputs.

# Question d

It is not a bad idea but somehow confusing to make
the strictness of a sequence depend on the inner procedure\'s strictness.

It might be little more verbose, but I think we should use
laziness explicitly rather than relying on the underlying mechanism.
That is, make good use of `delay` and `force`.
