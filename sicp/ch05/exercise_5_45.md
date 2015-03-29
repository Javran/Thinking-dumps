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
