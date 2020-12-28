module SumOfMultiples
    ( sumOfMultiples
    , sumOfMultiplesDefault
    )
where

import qualified Data.List.Ordered as OL

sumOfMultiples :: [Int] -> Int -> Int
sumOfMultiples xs upBound = sum $ takeWhile (< upBound) multiples
    where
        --   [a,b,c...] =>
        -- [ [a,2a,3a,...]
        -- , [b,2b,3b,...]
        -- , ...
        -- ]
        xsMultiples = map (\x -> iterate (+ x) x) xs
        -- ordered list merge, thanks to laziness.
        multiples = foldr OL.union [] xsMultiples

sumOfMultiplesDefault :: Int -> Int
sumOfMultiplesDefault = sumOfMultiples [3,5]
