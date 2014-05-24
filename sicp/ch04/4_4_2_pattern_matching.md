Pattern matcher: the program that tests whether
some datum fits a specified pattern.

Input to the pattern matcher:

* a pattern
* a datum
* a frame (specifies bindings for various pattern variables)

The pattern matcher checks whether the datum matches the
pattern in a way that is consistent
with the bindings already in the frame.

My note: frames are playing important roles
in this process. If we are doing pattern matching
on empty frame, then all possible combinators will be acceptable.
But if the frame is non-empty, some solutions might be rolled
out because of the inconsistency.
And also I think the frame will be accumuated throughout this pattern matching
process.
