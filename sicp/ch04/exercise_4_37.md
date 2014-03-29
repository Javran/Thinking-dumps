Ben is correct. The program in exercise 4.35 will
try to search `i`, `j` and `k` where `low <= i <= j <= high`.

But in this exercise, only `i` and `j` is attempted because
we can calculate `k` directly instead of searching all the possible
values.

Some optimization is done as well,
for example, the requirement `hsq >= ksq` eliminiates
some "too big" value of `j` to make sure `k` is within range
before going down to the deeper search space.

However, these optimizations does not do much in terms of
narrowing down the search space, what is really important
is that we no longer need to blindly try `k`, which is a big win.

FYI: I wrote a program in Haskell to demonstrate the similarity
between "list monad" and "amb". Please check out `./exercise_4_37.hs`.
