{-# LANGUAGE ScopedTypeVariables #-}
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

largestPalindrome l h = fastSmallestPalindrome Down getDown pred h l
smallestPalindrome = fastSmallestPalindrome id id succ

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

fastSmallestPalindrome :: forall a b .(Integral a, Ord b) => (a -> b) -> (b -> a) -> (a->a) -> a -> a -> (a, [(a,a)])
fastSmallestPalindrome toOrd fromOrd next vLow vHigh = (fromOrd a, map (\(x,y) -> (fromOrd x, fromOrd y)) . S.toList $ b)
  where
    search :: b -> b -> b -> Maybe (b, S.Set (b,b)) -> Maybe (b, S.Set (b,b))
    search v1 v2From v2To curBest
        | v2From > v2To = curBest
        | maybe False (\(vMin,_) -> vMin < vProd) curBest = curBest
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

    search2 :: b -> b -> Maybe (b, S.Set (b,b)) -> Maybe (b, S.Set (b,b))
    search2 v1From v1To curBest
        | v1From > v1To = curBest
        | maybe False (\(vMin,_) -> vMin < vProd) curBest = curBest
        | otherwise = search2 (toOrd . next . fromOrd $ v1From) v1To (search v1From v1From (toOrd vHigh) curBest)
      where
        vProdI = fromOrd v1From * fromOrd v1From
        vProd = toOrd vProdI

    Just (a,b) = search2 (toOrd vLow) (toOrd vHigh) Nothing
