{-# LANGUAGE LambdaCase #-}
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

-- traverse a Music structure, by a function on Primitive, and concatenate results.
-- it is assumed that this Music is composed from a `line` function and does not consist of (:=:).
traverseLined :: (Primitive a -> [b]) -> Music a -> [b]
traverseLined f mp = case mp of
  Prim prim -> f prim
  Modify _ m -> traverseLined f m
  m0 :+: m1 -> traverseLined f m0 <> traverseLined f m1
  _ :=: _ ->
    -- I believe in some cases it is not impossible to implement this for :=: case,
    -- but I can imagine it being complicated and exercise does not require us to implement this anyway.
    []

{- Ex 6.2 -}
properRow :: Music Pitch -> Bool
properRow = checkProper . traverseLined extractPc
  where
    extractPc prim =
      case prim of
        Note d (pc, _) ->
          -- rest are allowed, therefore we should ignore notes that have no time length.
          [pc | d /= 0]
        Rest _ -> []

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
palin = isPalindrome . traverseLined extractNote
  where
    extractNote prim = case prim of
      Note _ (pc, _) -> [pc]
      Rest {} -> []

{-
  TODO:

  - we'll definitely need some testing.

 -}

{- not really useful for now, but an isomorphism can be observed here. -}
unpairPrim :: Primitive a -> (Dur, Maybe a)
unpairPrim = \case
  Note d v -> (d, Just v)
  Rest d -> (d, Nothing)

pairPrim :: (Dur, Maybe a) -> Primitive a
pairPrim (d, m) = case m of
  Nothing -> Rest d
  Just v -> Note d v

{- Ex 6.4: TODO -}
retroPitches :: Music Pitch -> Music Pitch
retroPitches =
    line . _z . unzip . traverseLined (pure . unpairPrim)
  where
    _z = undefined
      -- this is not quite working - as traverseLined destroys the structure,
      -- we have no way of re-constructing it back.
