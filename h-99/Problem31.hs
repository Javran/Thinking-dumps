module Problem31 where

import Control.Arrow
import Data.List (nubBy)

primes :: (Integral a) => [a]
primes = nubBy (\x y -> x `mod` y == 0) [2..]

isPrime :: (Integral a) => a -> Bool
isPrime n
   | n <= 1 = False
   | otherwise = all ((/= 0) . (n `mod`)) primes'
    where
        hi = floor (sqrt (fromIntegral n :: Double))
        primes' = takeWhile (<= hi) primes

main :: IO ()
main = mapM_ (print . (id &&& isPrime)) [1..20 :: Int]
