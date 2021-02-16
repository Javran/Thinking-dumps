module Accumulate
    (accumulate)
where

accumulate :: (a -> b) -> [a] -> [b]
accumulate f = foldr (\i acc -> f i:acc) []
