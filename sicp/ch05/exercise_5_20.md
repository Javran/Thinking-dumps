The first line:

    (define x (cons 1 2))

Will cause `free` to increase by 1 and goes to `p2`.

`Location` | `the-car` | `the-cdr`
--- | --- | ---
`1` | `n1` | `n2`

The second line is equivalent to:

    (define y (cons x (cons x '())))

For the inner `(cons x '())`:

`Location` | `the-car` | `the-cdr`
--- | --- | ---
`1` | `n1` | `n2`
`2` | `p1` | `e0`

For the outer `cons`:

`Location` | `the-car` | `the-cdr`
--- | --- | ---
`1` | `n1` | `n2`
`2` | `p1` | `e0`
`3` | `p1` | `p2`

Final values:

* `free = p4`
* `x = p1`
* `y = p3`
