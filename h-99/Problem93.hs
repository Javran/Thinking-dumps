module Problem93 where

import Control.Arrow

-- | all possible ways of splitting a list into 2 parts
splits :: [a] -> [([a],[a])]
splits [] = [([],[])]
splits as@(x:xs) = ([],as) : map (first (x:)) (splits xs)
