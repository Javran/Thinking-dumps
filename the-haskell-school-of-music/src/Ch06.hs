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
    extractPc (Prim prim) = case prim of
      Note d (pc, _) ->
        -- rest are allowed, therefore we should ignore notes that have no time length.
        [pc | d /= 0]
      Rest _ -> []
    extractPc (Modify _ m) = extractPc m -- not necessary, just best effort.
    extractPc (m0 :+: m1) = extractPc m0 <> extractPc m1
    extractPc (_ :=: _) =
      -- I guess what exercise meant is that we can assume this construction does not appear
      -- as an input value to this function. So here :=: is marked explicitly as empty to say
      -- that this is unexpected.
      []

    checkProper xs =
      -- has exactly 12 notes
      length xs == 12
        &&
        -- each unique pitch class used exactly once
        length (nub xs) == 12

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = and $ zipWith (==) (take half xs) (reverse xs)
  where
    half = l `quot` 2
    l = length xs

{- Ex 6.3 -}
palin :: Music Pitch -> Bool
palin = isPalindrome . concatMap extractNote . lineToList
  where
    extractNote (Prim prim) = case prim of
      Note _ (pc, _) -> [pc]
      Rest {} -> []
    extractNote (Modify _ m) = extractNote m
    extractNote (m0 :+: m1) = extractNote m0 <> extractNote m1
    extractNote (_ :=: _) =
      -- this is possible to implement,
      -- just that it'll be a bit complicated and not really required by this exercise.
      []
