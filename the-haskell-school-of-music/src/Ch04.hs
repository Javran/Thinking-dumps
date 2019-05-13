{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Ch04 where

import Euterpea hiding (times)

twinkle :: Music Pitch
twinkle = line1 [pat0, pat1, pat1, pat0]
  where
    pat0 = mkLine [C,C,G,G,A,A,G] :+: mkLine [F,F,E,E,D,D,C]
    pat1 = mkLine [G,G,F,F,E,E,D]
    mkLine =
      line1
      . zipWith (\d pc -> note d (pc,4)) (replicate 6 qn <> [hn])

times :: Int -> Music a -> Music a
times n m
  | n < 0 = error "expected non-negative input"
  | n == 0 = rest 0
  | n == 1 = m
  | otherwise = case n `quotRem` 2 of
      (n',0) -> let half = times n' m in half :+: half
      (n',1) -> let half = times n' m in m :+: half :+: half
      _ -> error "unreachable"
