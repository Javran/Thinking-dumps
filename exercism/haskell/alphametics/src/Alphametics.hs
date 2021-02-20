module Alphametics
  ( solve
  )
where

import Control.Monad
import Data.Char
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List
import Data.Maybe
import qualified Data.Set as S
import Text.ParserCombinators.ReadP

{-
  Note: this passes normal tests and some additional ones, but does not support
  multiplication or exponents, which will require more work.
 -}

-- LHS and RHS of a puzzle equation.
-- note that the string is reversed so the least significant digit is the first one.
type Equation = ([String], String)

parse :: String -> Maybe (Equation, S.Set Char)
parse raw = case readP_to_S (equation <* eof) raw of
  [(v, "")] -> pure v
  _ -> Nothing
  where
    equation = do
      let pzWord :: ReadP String
          pzWord = munch1 isAsciiUpper
      lhs <- pzWord `sepBy1` string " + "
      _ <- string " == "
      rhs <- pzWord
      pure
        ( (fmap reverse lhs, reverse rhs)
        , S.fromList $ take 1 rhs <> concatMap (take 1) lhs
        )

-- only key 0 ~ 25 are used for 'A' ~ 'Z', value represents # of times it appeared.
type PackedDigit = IM.IntMap Int

-- from least significant digit to most, each element represents <set of lhs chars>, <rhs>
type PackedEquation = [(PackedDigit, Int)]

chOrd :: Char -> Int
chOrd ch = ord ch - ord 'A'

packEquation :: Equation -> PackedEquation
packEquation = unfoldr go
  where
    go (_, []) = Nothing
    go (ls, r : rs) = do
      let lsPairs = fmap (splitAt 1) ls
          ls' = concatMap (\(_, xs) -> [xs | not (null xs)]) lsPairs
          curDigit :: PackedDigit
          curDigit = IM.fromListWith (+) $ do
            (ds, _) <- lsPairs
            d <- ds
            pure (chOrd d, 1)
      pure ((curDigit, chOrd r), (ls', rs))

solve :: String -> Maybe [(Char, Int)]
solve puzzle = do
  (equ@(lhs, rhs), nonZeros) <- parse puzzle
  let rhsLen = length rhs
  -- eliminate an impossible case first: no word from LHS can be longer than RHS.
  guard $ all ((<= rhsLen) . length) lhs
  let packed = packEquation equ
      nonZeroIndices = S.map chOrd nonZeros
      initSearchPlan :: [Int]
      initSearchPlan = nub $ do
        -- from least significant to most,
        -- fill lhs followed by rhs.
        (l, r) <- packed
        IM.keys l <> [r]
      verify :: IM.IntMap Int -> (PackedDigit, Int) -> Int -> Maybe Int
      verify assignments (ls, r) carry = do
        sumLhs <-
          foldM
            (\acc (ind, cnt) -> do
               v <- assignments IM.!? ind
               pure (acc + v * cnt))
            0
            $ IM.toList ls
        rhsVal <- assignments IM.!? r
        let (lhsQ, lhsR) = (sumLhs + carry) `quotRem` 10
        guard $ lhsR == rhsVal
        pure lhsQ
      dfs assignments usedDigits searchPlan equation carry
        | null equation = assignments <$ guard (carry == 0)
        | otherwise = case searchPlan of
          [] -> do
            Just carry' <- pure $ verify assignments (head equation) carry
            dfs assignments usedDigits searchPlan (tail equation) carry'
          (curInd : searchPlan') -> do
            let candidates =
                  [ i
                  | i <-
                      [ if curInd `elem` nonZeroIndices then 1 else 0
                      .. 9
                      ]
                  , IS.notMember i usedDigits
                  ]
            d <- candidates
            let usedDigits' = IS.insert d usedDigits
                assignments' = IM.insert curInd d assignments
                filled :: Maybe ()
                filled = do
                  let (xs, y) = head equation
                  forM_ (y : IM.keys xs) $ \k -> do
                    () <$ assignments IM.!? k
            case filled of
              Nothing ->
                dfs assignments' usedDigits' searchPlan' equation carry
              Just () ->
                case verify assignments' (head equation) carry of
                  Nothing -> []
                  Just carry' ->
                    dfs assignments' usedDigits' searchPlan' (tail equation) carry'
  result <- listToMaybe $ dfs IM.empty IS.empty initSearchPlan packed 0
  pure $ (\(k, v) -> (chr (k + ord 'A'), v)) <$> IM.toList result
