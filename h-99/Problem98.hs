{-# LANGUAGE FlexibleContexts #-}
module Problem98 where

import Control.Monad
import Data.Maybe

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

mkRule :: [Int] -> Rule
mkRule xs = Rule xs $ sum xs + length xs - 1

mkRowRule, mkColRule :: Int -> [Int] -> RCRule

mkRowRule i xs = (Left i, mkRule xs)
mkColRule i xs = (Right i, mkRule xs)

ruleView :: Rule -> Maybe (Int, Rule)
ruleView (Rule [] _) = Nothing
ruleView (Rule [x] _) = Just (x, Rule [] 0)
ruleView (Rule (x:xs) l) = Just (x, Rule xs (l-x-1))

solveRule :: Rule -> [CellContent] -> [ [Bool] ]
solveRule r xs = case ruleView r of
    Nothing -> fst <$> checkedFill False (length xs) xs
    Just (curLen, r') -> case xs of
        [] -> mzero
        _:_ -> do
            startFill <- [True, False]
            if startFill
              then do
                (filled, remained) <- checkedFill True curLen xs
                case ruleView r' of
                    Nothing -> fst <$> checkedFill False (length remained) remained
                    _ -> do
                        (filled1, remained1) <- checkedFill False 1 remained
                        result <- solveRule r' remained1
                        pure $ filled ++ filled1 ++ result
              else _
  where
    checkedFill :: Bool -> Int -> [CellContent] -> [ ([Bool], [CellContent]) ]
    checkedFill b count ys
        | count == 0 = pure ([], ys)
        | otherwise = case ys of
            [] -> mzero
            (m : ys') -> do
                guard (maybe True (\b2 -> b == b2) m)
                (filled, remained) <- checkedFill b (count-1) ys'
                pure (b:filled, remained)

