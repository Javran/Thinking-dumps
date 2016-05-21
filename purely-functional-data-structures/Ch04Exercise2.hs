module Ch04Exercise2 where

import Data.List hiding (insert)

insert :: Ord a => a -> [a] -> [a]
insert v [] = [v]
insert v l@(x:xs)
    | v <= x = v : l
    | otherwise = x : insert v xs

insertionSort :: Ord a => [a] -> [a]
insertionSort = foldl' (flip insert) []
