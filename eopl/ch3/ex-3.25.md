The evaluation process might be too long to write down,
so here I just attempt to give my understanding:

# Implementing recursive function

To write a recursive function, we can basically do it in this way:

    let times4 =
      proc (x)
        if zero?(x)
          then 0
          else -((times4 -(x,1)),-4)

The problem is: the value of `times4` cannot be determined
when interpreting this expression, because variable `time4`
are waiting for the value of the expression after `=`, so it
is not available inside that expression.

So instread, we make `times4` accept another function which helps
it to complete the recursive call:

    let makemult =
      proc (f)
        proc (x)
          if zero?(x)
            then 0
            else -((f -(x,1)),-4)

We change its name to indicate that `makemult` is not recursive.
We need to feed it with another function to make it a real recursive
function.

Notice `f` is the real recursive function, we can now make one by feeding
`makemult` by some function, by apply this rule multiple times:

    let time4 = proc (x) ((makemult <??>) x)
    let time4 = proc (x) ((makemult (makemult <??>)) x)
    let time4 = proc (x) ((makemult (makemult (makemult <??>)) x))
    let time4 = ...

But wait, it is growing will never end.

Here comes the trick, that to tell the truth I would never come up with:

We allow `f` not to be a recursive function, but to be a function that 
`almost` recursive:

    let makemult =
      proc (f)
        proc (x)
          if zero?(x)
            then 0
            else -(((f f) -(x,1)),-4)

By applying `f` to itself, we are always dealing with an `almost` recursive
function, by now the expansion is automatically done for us.

Whenever we reach the line `-(((f f) -(x,1)),-1)`, `f` keeps applying itself
to itself, and in return gives us a recursive function.

This process can be terminate as expected, as long as the recursive function itself
does not end up in an infinity loop.

# More general way of implementing recursive function

Now I switch back to racket programming language for my convenience as
it is basically the same thing in PROC language.

Suppose we have a function `y` that accepts a curried binary function `f`,
and `y` has the property that
`((y f) x) = ((f (y f)) x)` (i.e.  `(y f) = (f (y f))`).

Now think `f` is that `almost recursive` function, then `(y f)` actually
gives us the recursive function corresponding to `f`.

(See part 1 from `./ex-3.25-y-combinator.rkt` for examples)

# Y combinator


