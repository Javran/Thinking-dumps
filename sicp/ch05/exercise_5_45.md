**Question a**

This is the recursive factorial function used in my measurement:

```scheme
(define (factorial n)
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))
```

The ratio of the number of pushes in the compiled version
to the number of pushes in the interpreted version is close to
`0.219`.

The ratio of the maximum stack depth in the compiled version
to the ratio of the maximum stack depth in the interpreted version is close to
`0.998`.

Please see `exercise_5_45.odt` for the original data.

Some analysis on the result shows that, if the input number is `n`,
for `n >= 2`:

* Number of pushes for the compiled version is `7n`.
* Number of pushes for the interpreted version is `32n-16`
* Number of pushes for the special-purpose factorial machine is `2n-1`

* Maximum stack depth for the compiled version is `3n-1`
* Maximum stack depth for the interpreted version is `3n+5`
* Maximum stack depth for the special-purpose factorial machine is `2n-1`

* The ratio constant (compiled:interpreted)
for number of pushes (when `n` is large) is: `0.21875`
* The ratio constant (compiled:interpreted)
for maximum stack depth (when `n` is large) is: `1`
* The ratio constant (special-purpose:interpreted)
for number of pushes (when `n` is large) is: `0.0625`
* The ratio constant (special-purpose:interpreted)
for maximum stack depth (when `n` is large) is: `0.667`
* The ratio constant (special-purpose:compiled)
for number of pushes (when `n` is large) is: `0.2857`
* The ratio constant (special-purpose:compiled)
for maximum stack depth (when `n` is large) is: `0.667`

**Question b**

TODO
