module Problem34 where

import Problem33 (coprime)

totient :: Int -> Int
totient m = length [ i | i <- [1..m-1], coprime i m ]

main :: IO ()
main = print $ totient 10
