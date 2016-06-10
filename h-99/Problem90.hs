module Problem90 where

import Control.Monad

-- forget about compact representation, let's focus on using full coordinate first

-- partial: a partial solution (no conflict)
-- lp: length of the parital solution
-- candidates (a list of row numbers not yet picked up
queens' :: [(Int,Int)] -> Int -> [Int] -> [ [(Int,Int)] ]
queens' partial _ [] = pure partial
queens' partial lp candidates = do
    let curCol = lp+1
    curRow <- candidates
    guard $ all (\(c,r) -> c+r /= curCol+curRow && c-r /= curCol-curRow) partial
    -- TODO: should have better ways to split selected candidate and remaining parts
    queens' ((curCol,curRow):partial) curCol (filter (/= curRow) candidates)


-- try:
-- > queens' [] 0 [1..8]
-- > queens' [] 0 [1..9]
-- TODO: now that we need a wrapper function around it.
