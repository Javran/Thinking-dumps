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

import Data.List

data TypeAB = A | B

instance Eq TypeAB where
    A == A = True
    B == B = True
    _ == _ = False

instance Ord TypeAB where
    t1 `compare` t2
        | t1 == t2 = EQ
        | t1 == A  = LT -- t2 can only be B, we define A < B
        | otherwise = GT -- t1 can only be B, and t2 can only be A

instance Show TypeAB where
    show A = "<A>"
    show B = "<B>"

instance Read TypeAB where
    readsPrec _ str
        | "<A>" `isSuffixOf` str = [(A, drop 3 str)]
        | "<B>" `isSuffixOf` str = [(B, drop 3 str)]
        | otherwise = []

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
    exampleTypeClass "Show"
    print A -- <A>
    print B -- <B>
    exampleTypeClass "Read"
    print (read "<A>" :: TypeAB)
    print (read "<B>" :: TypeAB)
    -- would raise error if the string cannot be parsed
    print (reads "No" :: [(TypeAB, String)])
    where
        exampleTypeClass tpc = putStrLn ("typeclass: " ++ tpc)
