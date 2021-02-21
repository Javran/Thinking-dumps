{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ViewPatterns #-}

module Change
  ( findFewestCoins
  )
where

import Data.Foldable
import Data.List.Extra (nubSort)
import Data.Maybe
import Data.MemoTrie (memoFix)
import Data.Ord

findFewestCoins :: Integer -> [Integer] -> Maybe [Integer]
findFewestCoins target (nubSort -> coins) =
  fmap fst $ listToMaybe $ search target
  where
    search = memoFix $ \f n ->
      if
          | n < 0 -> []
          | n == 0 -> pure ([], 0 :: Int)
          | otherwise ->
            let alts = do
                  c <- takeWhile (<= n) coins
                  (xs, l) <- f (n - c)
                  pure (c : xs, l + 1)
             in [minimumBy (comparing snd) alts | not (null alts)]
