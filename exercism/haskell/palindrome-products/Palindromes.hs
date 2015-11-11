module Palindromes
  ( largestPalindrome
  , smallestPalindrome
  ) where

import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Data.Ord

{-

We have 2 approaches:

* the most naive one attempts all products
* using the property of the search space helps us to
  reduce number of attempts needed: if we are finding the smallest
  palindrome, and we know that a*b is one candidate,
  then searching a*(b+1), a*(b+2) ... won't give us better solutions
  so we can stop right here and continue with attempting (a+1)* ???

-}

largestPalindrome, smallestPalindrome :: Integral a => a -> a -> (a, [(a,a)])

largestPalindrome = naiveSmallestPalindrome Down getDown
smallestPalindrome = naiveSmallestPalindrome id id

isPalindrome :: Integral a => a -> Bool
isPalindrome v = and (zipWith
                       (==)
                       (take (l `quot` 2) xs)
                       (reverse xs))
  where
    xs = show (toInteger v)
    l = length xs

getDown :: Down a -> a
getDown (Down v) = v

naiveSmallestPalindrome :: (Integral a, Ord b)
                           -- conversion from and to Ord instances
                        => (a -> b) -> (b -> a)
                        -> a -> a -> (a, [(a,a)])
naiveSmallestPalindrome toOrd fromOrd vLow vHigh = (fromOrd targetKey, S.toList targetSet)
  where
    products = [ (a,b)
               | a <- [vLow .. vHigh]
               , b <- [a .. vHigh]
               ]
    collects = M.fromListWith
                 S.union
                 (filter (isPalindrome . fromOrd . fst) $
                  map
                  (\p@(a,b) -> (toOrd (a*b), S.singleton p))
                  products)
    (targetKey, targetSet) = M.findMin collects

