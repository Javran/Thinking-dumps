module DNA
    ( hammingDistance
    )
where

-- | (.:) use its first argument to modify the result
--   of its second argument (a binary),
--   resulting in another binary function
--   (semantic editor combinator)
(.:) :: (a -> b)      -- ^ modify the result of the orignal function
     -> (c -> d -> a) -- ^ the original binary function
     ->  c -> d -> b
-- (.:) = (.) . (.)
(f .: g) x y = f (g x y)

-- | calculate the hamming distance between two strings
hammingDistance :: String -> String -> Int
hammingDistance = countMismatch .: pairwiseCompare
    where
        pairwiseCompare = zipWith (==)
        countMismatch = length . filter not
