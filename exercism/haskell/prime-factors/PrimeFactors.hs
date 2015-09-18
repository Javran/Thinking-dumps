module PrimeFactors
  ( primeFactors
  ) where

-- | list difference, assuming the input list is ordered
orderedDiff :: Ord a => [a] -> [a] -> [a]
orderedDiff [] _ = []
orderedDiff as [] = as
orderedDiff x@(a:as) y@(b:bs) = case a `compare` b of
  LT -> a : orderedDiff as y
  EQ -> orderedDiff as bs
  GT -> orderedDiff x bs

primeFactors :: Integral a => a -> [a]
primeFactors = undefined
