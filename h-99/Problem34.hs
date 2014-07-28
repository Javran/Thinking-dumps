module Problem34 where

import Problem33 (coprime)

totient :: Int -> Int
totient 1 = 1
totient m = length [ i | i <- [1..m-1], coprime i m ]

main :: IO ()
main = do
    print $ totient 10
    print $ totient 1
