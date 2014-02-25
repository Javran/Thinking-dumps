Suppose `unless` is not a special form but just an applicative-order function,
then the evaluation of `(factorial 5)` would never terminate.
Because before `unless` is entered, we have to evaluate
its two arguments first, which causes the "alternative" part
to be evaluated even if we don't use it at all.

However, this definition should work in normal-order languages.
Because the arguments are evaluate only as needed.
