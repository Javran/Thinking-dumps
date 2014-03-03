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


