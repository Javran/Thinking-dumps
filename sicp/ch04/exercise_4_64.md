The original rule is:

    (rule (outranked-by ?staff-person ?boss)
          (or (supervisor ?staff-person ?boss)
              (and (supervisor ?staff-person ?middle-manager)
                   (outranked-by ?middle-manager ?boss))))

And the one that has mistakes is:

    (rule (outranked-by ?staff-person ?boss)
          (or (supervisor ?staff-person ?boss)
              (and (outranked-by ?middle-manager ?boss)
                   (supervisor ?staff-person ?middle-manager))))

The only difference is the ordering.
Just like what happens in a normal procedure,
we will get an infinite loop simply because
the variable `?middle-manager` is free at its first occurrence,
so this rule always matches itself and goes to its body
in order to match other variables,
which eventually results in an infinite loop.

TODO: need some specific example about the question describe
in the exercise.
