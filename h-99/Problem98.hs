{-# LANGUAGE FlexibleContexts #-}
module Problem98 where

import Control.Monad
import Data.Maybe
import Data.Foldable
import Data.Function
import Data.List

data Rule = Rule
  { ruleLens :: [Int] -- lengths
    -- calculated from ruleLens, the length of the most compact solution
    -- satisfying this rule.
  , ruleAtLeast :: !Int
  } deriving (Show)

-- Left: Row rule
-- Right: Col rule
type RCRule = (Either Int Int, Rule)
type CellContent = Maybe Bool
data Nonogram = NG !Int !Int [RCRule]

minViewBy :: (a -> a -> Ordering) -> [a] -> Maybe (a,[a])
minViewBy _ [] = Nothing
minViewBy f xs = Just . minimumBy (f `on` fst) $ xsWithContext
  where
    xsWithContext = zip xs (zipWith (++)
                              (init $ inits xs)
                              (tail $ tails xs))

mkRule :: [Int] -> Rule
mkRule xs = Rule xs $ sum xs + length xs - 1

mkRowRule, mkColRule :: Int -> [Int] -> RCRule

mkRowRule i xs = (Left i, mkRule xs)
mkColRule i xs = (Right i, mkRule xs)

ruleView :: Rule -> Maybe ((Int, Int), Rule)
ruleView (Rule [] _) = Nothing
ruleView (Rule [x] l) = Just ((x,l), Rule [] 0)
ruleView (Rule (x:xs) l) = Just ((x,l), Rule xs (l-x-1))

solveRule :: Rule -> [CellContent] -> [ [Bool] ]
solveRule r1 xs1 = map tail (solveRule' r1 (Nothing:xs1))
  where
    -- TODO: let's say to satisfy the next new rule we always fill in a "False" as the separator
    -- and caller of this function should be responsible for prepending a Nothing in front of the [CellContent]
    solveRule' :: Rule -> [CellContent] -> [ [Bool] ]
    solveRule' r xs = case ruleView r of
        -- all rules have been satisfied, we fill rest of the cells with False
        Nothing -> fst <$> checkedFill False (length xs) xs
        -- now we are trying to have one or more "False" and "curLen" consecutive "True"s
        Just ((curLen,leastL), r') -> do
            -- we can fail immediately here if we have insufficient number of cells.
            guard $ length xs >= leastL + 1
            -- always begin with one "False"
            (filled1,remained1) <- checkedFill False 1 xs
            -- now we have 2 options, either start filling in these cells, or
            startFill <- [True, False]
            if startFill
                then do
                    (filled2, remained2) <- checkedFill True curLen remained1
                    filled3 <- solveRule' r' remained2
                    pure (filled1 ++ filled2 ++ filled3)
                else do
                    filled2 <- solveRule' r remained1
                    pure (filled1 ++ filled2)
    -- "checkedFill b count ys" tries to fill "count" number of Bool value "b"
    -- into cells, results in failure if cell content cannot match with the indended value.
    checkedFill :: Bool -> Int -> [CellContent] -> [ ([Bool], [CellContent]) ]
    checkedFill b count ys
        | count == 0 =
            -- no need to fill in anything, done.
            pure ([], ys)
        | otherwise = case ys of
            [] ->
                -- there's no room for filling anything, results in failure
                mzero
            (m:ys') -> do
                -- if the cell has not yet been filled, there's no problem.
                -- otherwise the already-existing value should match what
                -- we are filling in.
                guard (maybe True (\b2 -> b == b2) m)
                (filled, remained) <- checkedFill b (count-1) ys'
                pure (b:filled, remained)
