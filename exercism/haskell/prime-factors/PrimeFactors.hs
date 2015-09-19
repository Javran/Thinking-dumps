{-# LANGUAGE ScopedTypeVariables #-}
module PrimeFactors
  ( primeFactors
  , main
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

-- | @isPrime primes n@ assumes that 'primes' is a sorted list
-- | and contains at least one factor of 'n'
isPrime :: forall a .Integral a => [a] -> a -> Bool
isPrime primes n = all (\k -> n `rem` k /= 0) primes'
  where
    primes' = takeWhile (\x -> x*x <= n) primes

primeFactors :: Integral a => a -> [a]
primeFactors = undefined

main :: IO ()
main = do
    let primes = genPrimes [2..]
    print (filter (isPrime primes) [2..200])
    print (take 200 primes)
