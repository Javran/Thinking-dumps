module Sieve
  ( primesUpTo
  ) where

import Data.List.Ordered

primesUpTo :: Integral a => a -> [a]
primesUpTo m = genPrimes [2..m]

genPrimes :: Integral a => [a] -> [a]
genPrimes [] = []
genPrimes (x:xs) = x : genPrimes (xs `minus` [x,x+x..])
