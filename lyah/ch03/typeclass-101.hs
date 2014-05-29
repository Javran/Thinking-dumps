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

data TypeAB = A | B
    deriving (Show)

instance Eq TypeAB where
    A == A = True
    B == B = True
    _ == _ = False

instance Ord TypeAB where
    t1 `compare` t2
        | t1 == t2 = EQ
        | t1 == A  = LT -- t2 can only be B, we define A < B
        | otherwise = GT -- t1 can only be B, and t2 can only be A

main :: IO ()
main = do
    exampleTypeClass "Eq"
    print (A /= B) -- True
    print (A == A) -- True
    print (A /= A) -- False
    exampleTypeClass "Ord"
    print (A < B) -- True
    print (B >= A) -- True
    print (B < A) -- False
    where
        exampleTypeClass tpc = putStrLn ("typeclass: " ++ tpc)
