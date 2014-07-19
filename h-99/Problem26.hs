module Problem26 where

import Control.Arrow

pickOne :: [a] -> [(a,[a])]
pickOne [] = []
pickOne (x:xs) = (x,xs) : map (second (x:)) (pickOne xs)

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations n xs = do
    (x,ys) <- pickOne xs
    zs <- combinations (n-1) ys
    return (x:zs)

main :: IO ()
main = print $ combinations 3 "abcdef"
