module Problem40 where

import Problem31 (primes,isPrime)

goldbach :: Int -> (Int, Int)
goldbach n = head [(p,n-p) | p <- primes', isPrime (n-p)]
    where
        primes' = takeWhile (< n) primes

main :: IO ()
main = print $ goldbach 28
