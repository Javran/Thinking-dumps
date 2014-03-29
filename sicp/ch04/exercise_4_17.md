* Why is there an extra frame in the transformed program?

Because `let` expression is equivalent to an application to a `lambda`
with all right hand side values in the definition part being the arguments.

The extra frame is caused by the application to lambda.

* Explain why this difference in environment structure
can never make a difference in the behavior of a correct program.

Because the binding variables are merely used by interal definitions.
But here we need a definition of what is considered a "correct program".
Or otherwise, I have a counter example here:

    (define op +)
    (define (proc a)
      (define x (op 3 2 1))
      (define op -)
      (define y (op 3 2 1)))
      (- x y))

How can you expand the body of `proc` without changing its behavior?
The problem is the definition of `op` is changed between the definition of `x` and `y`.
We cannot simply mask `op` because the first `op` used in the definition of `x` is
different.

* Design a way to make the
interpreter implement the "simultaneous" scope rule for internal
definitions without constructing the extra frame.

We can use the same technique, but make the interpreter operate
on the frame of the procedure's arguments.
Scan internal definitions and mask the variables in the frame
where argument bindings are lied in.
