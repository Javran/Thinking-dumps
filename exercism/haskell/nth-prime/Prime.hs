module Prime where

-- | list difference, assuming the input list is ordered
orderedDiff :: Ord a => [a] -> [a] -> [a]
orderedDiff [] _ = []
orderedDiff as [] = as
orderedDiff x@(a:as) y@(b:bs) = case a `compare` b of
  LT -> a : orderedDiff as y
  EQ -> orderedDiff as bs
  GT -> orderedDiff x bs

-- | expect infinite list '[2,3...]' for sieving
genPrimes :: Integral a => [a] -> [a]
genPrimes [] = undefined
genPrimes (x:xs) = x : genPrimes (xs `orderedDiff` [x,x+x..])

primes :: [Int]
primes = genPrimes [2..]

nth :: Int -> Int
nth v = primes !! (v-1)
