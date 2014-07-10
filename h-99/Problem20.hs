module Problem20 where

import Control.Arrow ((&&&), second)

removeAt :: Int -> [a] -> (a,[a])
removeAt i xs
    | null xs = error "empty list"
    | i <= 0 || i > l = error "index out of range"
    | i == 1 = (head &&& tail) xs
    | otherwise = let (y:ys) = xs
                  in second (y:) . removeAt (i-1) $ ys
    where l = length xs

main :: IO ()
main = print $ removeAt 2 "abcd"

