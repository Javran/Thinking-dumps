From a functional point of view,
what application does is to substitute
parameters in the procedure body part with
the actual arguments. This means we have to fetch the body
of a procedure in the first place.
In other words, if the operator is a thunk,
there is nothing we can do about it.

Example:

    (define (add x y)
      (+ x y))

    ;; now try evaluating the expression:
    (add 10 20)

Here we have to evaluate `add` so that we can
bind arguments `10` and `20` to the corresponding parameters.
