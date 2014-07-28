module Problem39 where

import Problem31 (isPrime)

primesR :: Int -> Int -> [Int]
primesR l r = filter isPrime candicates
    where
        candicates = filter (\x -> x `mod` 2 /= 0
                                && x `mod` 3 /= 0
                                && x `mod` 5 /= 0)
                            [l..r]

main :: IO ()
main = print $ primesR 10 20
