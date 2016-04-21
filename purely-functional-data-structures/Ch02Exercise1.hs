module Ch02Exercise1 where

suffixes :: [a] -> [[a]]
suffixes [] = [[]]
suffixes l@(_:xs) = l : suffixes xs
