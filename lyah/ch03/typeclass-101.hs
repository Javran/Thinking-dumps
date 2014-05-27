{-
notes:

`=>` is called "class constraint"

Eq:
    used for types that supports equality tests
Ord:
    types that have an ordering

Show:
    can be presented as String

Read:
    can be read in from a String (actually parsers are used most of the time)
    (see also: "reads" function)

Enum:
    can be enumerated

Bounded:
    have both an upper and lower bound

Num:
    numerics

Integral:
    only whole numbers

Floating:
    floating point numbers

use "fromIntegral" to convert ints to floating numbers
-}

main :: IO ()
main = print "TODO"
