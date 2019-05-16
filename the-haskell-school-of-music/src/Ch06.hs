{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Ch06 where

import Data.List (nub)
import Euterpea

{-
  Ex 6.1:

  - all makes intuitive sense, not gonna carry out full proof,
    but can make some notes regarding them:

  - note that `line . lineToList == id` and `lineToList . line == id`,
    expanding `retro . retro` will make some line vs lineToList cancellings.

  - from same principle, expanding `retroInvert . invertRetro`
    will get everything cancelled out to get `retroInvert . invertRetro = id`

 -}

{- Ex 6.2 -}
properRow :: Music Pitch -> Bool
properRow = checkProper . (extractPc =<<) . lineToList
  where
    extractPc (Prim (Note _ (pc, _))) = [pc]
    extractPc (Modify _ m) = extractPc m -- not necessary, just best effort.
    extractPc _ = []

    checkProper xs =
      -- has exactly 12 notes
      length xs == 12 &&
      -- each unique pitch class used exactly once
      length (nub xs) == 12
