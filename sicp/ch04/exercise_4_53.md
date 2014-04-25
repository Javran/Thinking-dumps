First let's recall what is `prime-sum-pair`:

    (define (prime-sum-pair list1 list2)
      (let ((a (an-element-of list1))
            (b (an-element-of list2)))
        (require (prime? (+ a b)))
        (list a b)))

Here `an-element-of` can be replaced by `amb`.

Since there is an `(amb)` inside the body of `if-fail`,
the first expression always fails, but the difference
is that by using `permanent-set!` we have built up a list
of all solutions, which will be finally outputed by the
fallback value of `if-fail`.

The final result should be `(43 113 23)`.
