{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Ch04 where

import Euterpea

twinkle :: Music Pitch
twinkle = line1 [pat0, pat1, pat1, pat0]
  where
    pat0 = mkLine [C,C,G,G,A,A,G] :+: mkLine [F,F,E,E,D,D,C]
    pat1 = mkLine [G,G,F,F,E,E,D]
    mkLine =
      line1
      . zipWith (\d pc -> note d (pc,4)) (replicate 6 qn <> [hn])
