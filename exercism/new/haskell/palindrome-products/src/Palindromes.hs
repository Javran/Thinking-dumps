{-# LANGUAGE ScopedTypeVariables #-}
module Palindromes
  ( largestPalindrome
  , smallestPalindrome
  ) where

import qualified Data.Set as S
import Data.Ord
import Control.Arrow

{-

We have 2 approaches:

* the most naive one attempts all products
  (this approach is running rather slow on test cases)

* using the property of the search space helps us to
  reduce number of attempts needed: if we are finding the smallest
  palindrome, and we know that a*b is one candidate,
  then searching a*(b+1), a*(b+2) ... won't give us better solutions
  so we can stop right here and continue with attempting (a+1)* ???

-}

-- Data.Ord.Down flips the result of any Ord instances,
-- we can use it to prevent writing similar code twice.
-- All we need here is an extractor that extracts the data from Down

getDown :: Down a -> a
getDown (Down v) = v

largestPalindrome, smallestPalindrome :: Integral a => a -> a -> (a, [(a,a)])

largestPalindrome = smallestPalindrome' Down getDown pred
smallestPalindrome = smallestPalindrome' id id succ

-- | test if a non-negative number is palindrome number
isPalindrome :: Integral a => a -> Bool
isPalindrome v =
      and
    . take (l `quot` 2)
    . (zipWith (==) <$> id <*> reverse)
    $ xs
  where
    xs = show (toInteger v)
    l = length xs

{-
-- found on: http://stackoverflow.com/a/26316343/315302
-- even with some strictness mark,
-- performance is worse than my naive approach

isPalindrome' :: Integral a => a -> Bool
isPalindrome' x = reversal x == x

reversal :: Integral a => a -> a
reversal = go 0
  where
    go a 0 = a
    go !a b = let (q,r) = b `quotRem` 10
             in go (a*10 + r) q

-}

-- | "smallestPalindrome' toOrd fromOrd next l r" finds the smallest palindrome product
--   in given range (l & r), "toOrd" and "fromOrd" should convert numbers to an instance of Ord
--   and "next" should indicate how to continue after considering current number
smallestPalindrome' :: forall a b .(Integral a, Ord b)
                          -- conversion from and to Ord instances
                       => (a -> b) -> (b -> a)
                          -- how to get "next value"
                       -> (a -> a)
                       -> a -> a -> (a, [(a,a)])
smallestPalindrome' toOrd fromOrd next l r =
    let l' = toOrd l
        r' = toOrd r
    in if l' <= r'
         then smallestPalindromeIntern l' r'
         else smallestPalindromeIntern r' l'
  where
    smallestPalindromeIntern vLow vHigh = (fromOrd a, map (fromOrd *** fromOrd) . S.toList $ b)
      where
        -- inner search loop, we want to find v1,v2 such that (v1*v2) is a palindrome
        -- and try to update current best result if possible
        -- "search" assumes a fixed v1 and try v2 through v2From to v2To
        -- list comprehension might be capable of this but we really wants performance
        -- so finer grained control is intended
        search :: b -> b -> b
                  -- the following type stores current best number
                  -> Maybe (b, S.Set (b,b))
                  -> Maybe (b, S.Set (b,b))
        search v1 v2From v2To curBest
              -- stop searching if we have run out of candidates
            | v2From > v2To = curBest
              -- check current best result, if v1*v2 cannot do any better than it
              -- then v1*(v2+1), v1*(v2+2), ... won't either. so we can shortcut the search
            | Just (vMin, _) <- curBest, vMin < vProd = curBest
              -- when we find a potential candidate, update current best value if possible
            | isPalindrome vProdI = case curBest of
                Nothing -> continueSearch (Just (vProd, S.singleton (v1, v2From)))
                Just (vMin, pairs) -> case vMin `compare` vProd of
                    LT -> curBest
                    EQ -> continueSearch (Just (vMin, S.insert (v1, v2From) pairs))
                    GT -> continueSearch (Just (vProd, S.singleton (v1, v2From)))
            | otherwise = continueSearch curBest
          where
            continueSearch = search v1 (toOrd . next . fromOrd $ v2From) v2To
            vProdI = fromOrd v1 * fromOrd v2From
            vProd = toOrd vProdI

        -- outer search loop, attempts v1 through v1From to v1To
        search2 :: b -> b -> Maybe (b, S.Set (b,b)) -> Maybe (b, S.Set (b,b))
        search2 v1From v1To curBest
              -- stop & shortcut checks, similar to that of "search"
            | v1From > v1To = curBest
            | Just (vMin, _) <- curBest, vMin < vProd = curBest
            | otherwise = search2
                            (toOrd . next . fromOrd $ v1From)
                            v1To
                            (search v1From v1From vHigh curBest)
          where
            vProd = toOrd (fromOrd v1From * fromOrd v1From)

        Just (a,b) = search2 vLow vHigh Nothing
