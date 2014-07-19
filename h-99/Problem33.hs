module Problem33 where

import Problem32 (myGCD)

coprime :: Int -> Int -> Bool
coprime a b = (== 1) $ myGCD a b

main :: IO ()
main = print $ coprime 35 64
