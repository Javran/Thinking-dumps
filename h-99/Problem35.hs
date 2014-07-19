module Problem35 where

import Problem31 (primes)

primeFactors :: Int -> [Int]
primeFactors 1 = []
primeFactors n = case filter (\x -> n `mod` x == 0) primes' of
        [] -> [n]
        h:_ -> h : primeFactors (n `div` h)
    where
        primes' = takeWhile (\x -> x*x <= n) primes

main :: IO ()
main = print . primeFactors $ 315
