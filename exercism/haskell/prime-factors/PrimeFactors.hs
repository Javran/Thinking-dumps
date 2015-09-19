{-# LANGUAGE ScopedTypeVariables #-}
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

-- | expect infinite list '[2,3...]' for sieving
genPrimes :: Integral a => [a] -> [a]
genPrimes [] = undefined
genPrimes (x:xs) = x : genPrimes (xs `orderedDiff` [x,x+x..])

-- | calculate square root for Integer
-- | see: https://wiki.haskell.org/Generic_number_type#squareRoot
squareRoot :: Integer -> Integer
squareRoot 0 = 0
squareRoot 1 = 1
squareRoot n =
   let (^!)= (^) :: Num a => a -> Int -> a
       twopows = iterate (^!2) 2
       (lowerRoot, lowerN) =
           last $ takeWhile ((n>=) . snd) $ zip (1:twopows) twopows
       newtonStep x = div (x + div n x) 2
       iters = iterate newtonStep (squareRoot (div n lowerN) * lowerRoot)
       isRoot r  =  r^!2 <= n && n < (r+1)^!2
   in head $ dropWhile (not . isRoot) iters

-- | @isPrime primes n@ assumes that 'primes' is a sorted list
-- | and contains at least one factor of 'n'
isPrime :: forall a .Integral a => [a] -> a -> Bool
isPrime primes n = all (\k -> n `rem` k /= 0) primes'
  where
    sqRt = fromIntegral (squareRoot (toInteger n)) :: a
    primes' = takeWhile (<= sqRt) primes

primeFactors :: forall a. Integral a => a -> [a]
primeFactors = primeFactors' primeList
  where
    primeList = genPrimes [2 :: a ..]
    -- consume an ordered list of primes, and calculate
    -- prime factors
    primeFactors' :: [a] -> a -> [a]
    primeFactors' _ 1 = []
    primeFactors' primes@(p:ps) n
        | isPrime primes n =
          -- an optimization: if the current one is prime number
          -- then there is nothing to be done,
          -- the consumed prime list can be used for prime test
          -- because this number cannot be divided by numbers that
          -- have been dropped from the current prime list
            [n]
        | otherwise =
          -- take the smallest prime number and test
            let (q,r) = n `quotRem` p
            in if r == 0
                 then p:primeFactors' primes q -- "p" is one prime factor
                 else primeFactors' ps n -- "p" is not a prime factor, drop it.
    -- the list should be infinite, this case is not reachable
    primeFactors' [] _ = undefined
