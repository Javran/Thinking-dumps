module Problem94 where

import Data.List

{-
  TODO:
  - generate k-regular graph that has n nodes
  - find ways to eliminate isomorphic cases (hope the existing answer to P85 works)
-}

-- pick one element from the list with context (remained elements).
-- unlike "Utils.pick", "pickOne" will cut all elements
-- in front of the chosen element.
pickOne :: [a] -> [(a,[a])]
pickOne = map (\(x:xs) -> (x,xs)) . init . tails

-- pick n elements from a list with context (remained elements).
-- the resulting sequence of the chosen elements is always a subsequence
-- of the original input.
-- e.g. it's possible for "pickN _ [1..10]" to return "[1,4,5,6..]"
-- as one of its result, but returning "[1,4,6,5]" is not possible
-- because there's no way for "6,5" to be a proper subsequence of "[1..10]"
pickN :: Int -> [a] -> [([a],[a])]
pickN 0 xs = [([],xs)]
pickN n xs = do
    (y,ys) <- pickOne xs
    (z,zs) <- pickN (n-1) ys
    pure (y:z,zs)
