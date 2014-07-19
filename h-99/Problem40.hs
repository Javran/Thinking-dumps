module Problem40 where

import Problem31 (primes)

goldbach :: Int -> (Int, Int)
goldbach n = head [(p,n-p) | p <- primes', (n-p) `elem` primes']
    where
        primes' = takeWhile (< n) primes

main :: IO ()
main = print $ goldbach 28
