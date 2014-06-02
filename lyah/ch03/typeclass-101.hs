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
import Data.Maybe
-- import Data.Function (on)
import Data.Profunctor

newtype BinFunc a b = BinFunc
    { runBinFunc :: a -> a -> b }

instance Profunctor BinFunc where
    dimap f g (BinFunc bin) = BinFunc pbin
        where
            pbin a b = g (bin (f a) (f b))

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

instance Enum TypeAB where
    toEnum 0 = A
    toEnum 1 = B
    toEnum _ = error "TypeAB: out of range"

    fromEnum A = 0
    fromEnum B = 1

instance Bounded TypeAB where
    minBound = A
    maxBound = B

data Mod5
    = Zero
    | One
    | Two
    | Three
    | Four
    deriving (Eq, Show, Enum, Bounded)

mod5ToInt :: Mod5 -> Int
mod5ToInt = fromJust . (`elemIndex` [Zero .. Four])

intToMod5 :: Int -> Mod5
intToMod5 = ([Zero .. Four] !! ) . (`mod` 5)

instance Num Mod5 where
    fromInteger n = [Zero .. Four] !! (fromIntegral n `mod` 5)
    signum = const One
    abs = id
    negate = intToMod5 . (\x -> 5 - x) . mod5ToInt
    {-
    a + b = intToMod5 (a ^+^ b)
        where (^+^) = (+) `on` mod5ToInt

    a * b = intToMod5 (a ^*^ b)
        where (^*^) = (*) `on` mod5ToInt
    -}
    (+) = runBinFunc (dimap mod5ToInt intToMod5 (BinFunc (+)))
    (*) = runBinFunc (dimap mod5ToInt intToMod5 (BinFunc (*)))

test :: IO ()
test = do

    let univ = [Zero .. Four]
        tests = [ (x,y) | x <- univ, y <- univ ]
        test1 = map (\(x,y) -> (x,y,x+y)) tests
        test2 = map (\(x,y) -> (x,y,x*y)) tests
        test3 = map (\(x,y) -> (x,y,x-y)) tests
    putStrLn "test plus:"
    mapM_ print test1
    putStrLn "test mult:"
    mapM_ print test2
    putStrLn "test minu:"
    mapM_ print test3

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
    exampleTypeClass "Enum"
    print [A .. B]
    exampleTypeClass "Bounded"
    print (minBound :: TypeAB)
    print (maxBound :: TypeAB)
    test
    where
        exampleTypeClass tpc = putStrLn ("typeclass: " ++ tpc)
