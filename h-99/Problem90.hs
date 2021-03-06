{-# LANGUAGE ScopedTypeVariables #-}
module Problem90 where

import Control.Monad
import Data.List
import Utils

-- taking 2 steps to simplify things:
-- 1. forget about compact representation, let's focus on using full coordinate first
-- 2. given compact representation, can we do better than this?
--    not much, but we can get rid of storing "col" to save some space,
--    as they can be tracked while building up solutions.

-- | internal use only: given a partial solution (no conflict) and candidates,
--   and find a full solution.
--
-- arguments:
-- * partial: a partial solution (no conflict)
-- * lp: length of the parital solution
-- * candidates (a list of row numbers not yet picked up
queens' :: [Int] -> Int -> [Int] -> [ [Int] ]
queens' partial _ [] = pure partial
queens' partial lp candidates = do
    let curCol = lp+1
    (curRow,remaining) <- pick candidates
    guard $
      and $ zipWith
              (\c r -> c+r /= curCol+curRow && c-r /= curCol-curRow)
              [lp,lp-1 .. 1]
              partial
    queens' (curRow:partial) curCol remaining
{-
-- original implementation
queens' :: [(Int,Int)] -> Int -> [Int] -> [ [(Int,Int)] ]
queens' partial _ [] = pure partial
queens' partial lp candidates = do
    let curCol = lp+1
    (curRow,remaining) <- pick candidates
    guard $ all (\(c,r) -> c+r /= curCol+curRow && c-r /= curCol-curRow) partial
    queens' ((curCol,curRow):partial) curCol remaining
-- try:
-- > queens' [] 0 [1..8]
-- > queens' [] 0 [1..9]
-}

-- | return all solutions of placing n queens on a n x n board
queens :: Int -> [ [Int] ]
queens n = map reverse (queens' [] 0 [1..n])
